import RAPIER from '@dimforge/rapier3d-compat';
import readline from 'node:readline';

const FIXED_DT = 1 / 60;
const SNAP_EPS = 1e-4;

let physicsWorld = null;
let eventQueue = null;
let simTime = 0;

const bodies = new Map(); // wireId (number) -> { id, spec, rb, rbHandle, colliders, colliderHandles }
const colliderHandleToId = new Map(); // colliderHandle -> wireId
const lastSentSnap = new Map(); // wireId -> { pos, rot, vel, angVel }

function clamp(v, lo, hi) {
  return Math.max(lo, Math.min(hi, v));
}

function quatRotateVec(q, vx, vy, vz) {
  const ix = q.w * vx + q.y * vz - q.z * vy;
  const iy = q.w * vy + q.z * vx - q.x * vz;
  const iz = q.w * vz + q.x * vy - q.y * vx;
  const iw = -q.x * vx - q.y * vy - q.z * vz;
  return {
    x: ix * q.w + iw * -q.x + iy * -q.z - iz * -q.y,
    y: iy * q.w + iw * -q.y + iz * -q.x - ix * -q.z,
    z: iz * q.w + iw * -q.z + ix * -q.y - iy * -q.x,
  };
}

function quatSlerp(a, b, t) {
  if (t <= 0) return { x: a.x, y: a.y, z: a.z, w: a.w };
  if (t >= 1) return { x: b.x, y: b.y, z: b.z, w: b.w };

  let bx = b.x;
  let by = b.y;
  let bz = b.z;
  let bw = b.w;
  let cosHalfTheta = a.x * bx + a.y * by + a.z * bz + a.w * bw;

  if (cosHalfTheta < 0) {
    bx = -bx;
    by = -by;
    bz = -bz;
    bw = -bw;
    cosHalfTheta = -cosHalfTheta;
  }

  if (cosHalfTheta >= 1.0) {
    return { x: a.x, y: a.y, z: a.z, w: a.w };
  }

  const sqrSinHalfTheta = 1.0 - cosHalfTheta * cosHalfTheta;
  if (sqrSinHalfTheta <= 1e-12) {
    const s = 1 - t;
    const x = s * a.x + t * bx;
    const y = s * a.y + t * by;
    const z = s * a.z + t * bz;
    const w = s * a.w + t * bw;
    const len = Math.hypot(x, y, z, w) || 1;
    return { x: x / len, y: y / len, z: z / len, w: w / len };
  }

  const sinHalfTheta = Math.sqrt(sqrSinHalfTheta);
  const halfTheta = Math.atan2(sinHalfTheta, cosHalfTheta);
  const ratioA = Math.sin((1 - t) * halfTheta) / sinHalfTheta;
  const ratioB = Math.sin(t * halfTheta) / sinHalfTheta;
  return {
    x: a.x * ratioA + bx * ratioB,
    y: a.y * ratioA + by * ratioB,
    z: a.z * ratioA + bz * ratioB,
    w: a.w * ratioA + bw * ratioB,
  };
}

function createLeafColliderDesc(colSpec) {
  switch (colSpec.type) {
    case 'none':
      return null;
    case 'box':
      return RAPIER.ColliderDesc.cuboid(
        colSpec.size[0] * 0.5,
        colSpec.size[1] * 0.5,
        colSpec.size[2] * 0.5
      );
    case 'sphere':
      return RAPIER.ColliderDesc.ball(colSpec.radius);
    case 'cylinder':
      return RAPIER.ColliderDesc.cylinder(colSpec.height * 0.5, colSpec.radius);
    case 'capsule':
      return RAPIER.ColliderDesc.capsule(colSpec.height * 0.5, colSpec.radius);
    default:
      return null;
  }
}

function collectColliderDescs(colSpec, offsetPose = null) {
  if (!colSpec || colSpec.type === 'none') return [];
  if (colSpec.type === 'compound') {
    const out = [];
    for (const part of colSpec.parts || []) {
      const sub = collectColliderDescs(part.collider, part.pose);
      for (const d of sub) out.push(d);
    }
    return out;
  }
  const desc = createLeafColliderDesc(colSpec);
  if (!desc) return [];
  if (offsetPose) {
    desc.setTranslation(
      offsetPose.pos[0],
      offsetPose.pos[1],
      offsetPose.pos[2]
    );
    desc.setRotation({
      x: offsetPose.rot[0],
      y: offsetPose.rot[1],
      z: offsetPose.rot[2],
      w: offsetPose.rot[3],
    });
  }
  return [desc];
}

function resetWorld(gravityArr) {
  bodies.clear();
  colliderHandleToId.clear();
  lastSentSnap.clear();

  if (eventQueue) {
    eventQueue.free();
    eventQueue = null;
  }
  if (physicsWorld) {
    physicsWorld.free();
    physicsWorld = null;
  }
  physicsWorld = new RAPIER.World({
    x: gravityArr[0],
    y: gravityArr[1],
    z: gravityArr[2],
  });
  eventQueue = new RAPIER.EventQueue(true);
}

function removeBody(id) {
  const entry = bodies.get(id);
  if (!entry) return;
  for (const h of entry.colliderHandles) {
    colliderHandleToId.delete(h);
  }
  if (entry.rb && physicsWorld) {
    physicsWorld.removeRigidBody(entry.rb);
  }
  bodies.delete(id);
  lastSentSnap.delete(id);
}

function spawnBody(spec) {
  if (bodies.has(spec.id)) {
    removeBody(spec.id);
  }

  let rb = null;
  let rbHandle = null;
  const colliders = [];
  const colliderHandles = [];
  const colDescs = collectColliderDescs(spec.collider);

  if (colDescs.length > 0) {
    let rbDesc;
    if (spec.kind.type === 'static' || spec.kind.type === 'sensor') {
      rbDesc = RAPIER.RigidBodyDesc.fixed();
    } else if (spec.kind.type === 'kinematic') {
      rbDesc = RAPIER.RigidBodyDesc.kinematicPositionBased();
    } else {
      rbDesc = RAPIER.RigidBodyDesc.dynamic();
      rbDesc.setLinvel(spec.initialVel[0], spec.initialVel[1], spec.initialVel[2]);
      rbDesc.setLinearDamping(spec.linearDamping);
      rbDesc.setAngularDamping(spec.angularDamping);
      rbDesc.setGravityScale(spec.gravityScale);
      rbDesc.setCcdEnabled(!!spec.ccd);
      if (spec.lockRot) {
        rbDesc.lockRotations();
      }
    }
    rbDesc.setTranslation(spec.pose.pos[0], spec.pose.pos[1], spec.pose.pos[2]);
    rbDesc.setRotation({
      x: spec.pose.rot[0],
      y: spec.pose.rot[1],
      z: spec.pose.rot[2],
      w: spec.pose.rot[3],
    });
    rb = physicsWorld.createRigidBody(rbDesc);
    rbHandle = rb.handle;

    const groupBits = (spec.filterGroup ?? 0xffff) & 0xffff;
    const maskBits = (spec.filterMask ?? 0xffff) & 0xffff;
    const packedGroups = ((groupBits << 16) | maskBits) >>> 0;

    for (const colDesc of colDescs) {
      colDesc.setCollisionGroups(packedGroups);
      if (spec.events) {
        colDesc.setActiveEvents(RAPIER.ActiveEvents.COLLISION_EVENTS);
        colDesc.setActiveCollisionTypes(
          RAPIER.ActiveCollisionTypes.DEFAULT |
            RAPIER.ActiveCollisionTypes.KINEMATIC_FIXED
        );
      }
      if (spec.kind.type === 'sensor') {
        colDesc.setSensor(true);
      } else {
        colDesc.setFriction(spec.friction);
        colDesc.setRestitution(spec.restitution);
        if (spec.kind.type === 'dynamic' && spec.mass > 0) {
          colDesc.setMass(spec.mass / colDescs.length);
        }
      }
      const col = physicsWorld.createCollider(colDesc, rb);
      colliders.push(col);
      colliderHandles.push(col.handle);
      colliderHandleToId.set(col.handle, spec.id);
    }
  }

  bodies.set(spec.id, {
    id: spec.id,
    spec: {
      id: spec.id,
      kind: spec.kind,
      pose: spec.pose,
      drive: spec.drive,
      gravityScale: spec.gravityScale,
      linearDamping: spec.linearDamping,
      angularDamping: spec.angularDamping,
    },
    rb,
    rbHandle,
    colliders,
    colliderHandles,
  });
}

function applyBodyUpdate(u) {
  const entry = bodies.get(u.id);
  if (!entry) return;
  const spec = entry.spec;

  if (u.kinematicSpeed !== undefined && spec.kind.type === 'kinematic') {
    spec.kind.speed = u.kinematicSpeed;
  }

  if (u.pose !== undefined) {
    spec.pose = u.pose;
    if (spec.kind.type === 'static' || spec.kind.type === 'sensor') {
      if (entry.rb) {
        entry.rb.setTranslation(
          { x: u.pose.pos[0], y: u.pose.pos[1], z: u.pose.pos[2] },
          true
        );
        entry.rb.setRotation(
          {
            x: u.pose.rot[0],
            y: u.pose.rot[1],
            z: u.pose.rot[2],
            w: u.pose.rot[3],
          },
          true
        );
      }
    }
  }

  if (u.drive !== undefined) {
    spec.drive = u.drive;
    if (entry.rb && spec.kind.type === 'dynamic' && u.drive.type !== 'none') {
      entry.rb.wakeUp();
    }
  }

  if (u.gravityScale !== undefined) {
    spec.gravityScale = u.gravityScale;
    if (entry.rb && spec.kind.type === 'dynamic') {
      entry.rb.setGravityScale(u.gravityScale, true);
    }
  }

  if (u.linearDamping !== undefined) {
    spec.linearDamping = u.linearDamping;
    if (entry.rb && spec.kind.type === 'dynamic') {
      entry.rb.setLinearDamping(u.linearDamping);
    }
  }

  if (u.angularDamping !== undefined) {
    spec.angularDamping = u.angularDamping;
    if (entry.rb && spec.kind.type === 'dynamic') {
      entry.rb.setAngularDamping(u.angularDamping);
    }
  }
}

function executeCommands(commands) {
  if (!commands || commands.length === 0) return;
  for (const cmd of commands) {
    if (cmd.type === 'sound') continue;
    const entry = bodies.get(cmd.id);
    if (!entry || !entry.rb) continue;
    if (cmd.type === 'impulse') {
      entry.rb.applyImpulse(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'torqueImpulse') {
      entry.rb.applyTorqueImpulse(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'setVel') {
      entry.rb.setLinvel(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'setAngVel') {
      entry.rb.setAngvel(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'teleport') {
      const p = cmd.pose.pos;
      const r = cmd.pose.rot;
      entry.rb.setTranslation({ x: p[0], y: p[1], z: p[2] }, true);
      entry.rb.setRotation({ x: r[0], y: r[1], z: r[2], w: r[3] }, true);
    }
  }
}

function applyDrivesAndKinematics(dt) {
  const sortedIds = Array.from(bodies.keys()).sort((a, b) => a - b);
  for (const id of sortedIds) {
    const entry = bodies.get(id);
    if (!entry || !entry.rb) continue;
    const { spec, rb } = entry;

    if (spec.kind.type === 'kinematic') {
      const cur = rb.translation();
      const tx = spec.pose.pos[0];
      const ty = spec.pose.pos[1];
      const tz = spec.pose.pos[2];
      const dx = tx - cur.x;
      const dy = ty - cur.y;
      const dz = tz - cur.z;
      const dist = Math.hypot(dx, dy, dz);
      const maxStep = Math.max(0.001, spec.kind.speed) * dt;
      if (dist <= maxStep) {
        rb.setNextKinematicTranslation({ x: tx, y: ty, z: tz });
      } else {
        const s = maxStep / dist;
        rb.setNextKinematicTranslation({
          x: cur.x + dx * s,
          y: cur.y + dy * s,
          z: cur.z + dz * s,
        });
      }
      const cr = rb.rotation();
      const targetRot = {
        x: spec.pose.rot[0],
        y: spec.pose.rot[1],
        z: spec.pose.rot[2],
        w: spec.pose.rot[3],
      };
      const nextRot = quatSlerp(cr, targetRot, Math.min(1, dt * 8));
      rb.setNextKinematicRotation(nextRot);
      continue;
    }

    if (spec.kind.type !== 'dynamic') continue;
    const d = spec.drive;
    if (!d || d.type === 'none') continue;

    if (d.type === 'walk') {
      const curVel = rb.linvel();
      rb.setLinvel({ x: d.vel[0], y: curVel.y, z: d.vel[2] }, true);
    } else if (d.type === 'cameraWalk') {
      const curVel = rb.linvel();
      // Default heading -Z forward, +X right in headless mode
      rb.setLinvel({ x: d.strafe, y: curVel.y, z: -d.forward }, true);
    } else if (d.type === 'velocity') {
      rb.setLinvel({ x: d.linVel[0], y: d.linVel[1], z: d.linVel[2] }, true);
      rb.setAngvel({ x: d.angVel[0], y: d.angVel[1], z: d.angVel[2] }, true);
    } else if (d.type === 'force') {
      rb.resetForces(true);
      rb.resetTorques(true);
      rb.addForce({ x: d.force[0], y: d.force[1], z: d.force[2] }, true);
      rb.addTorque({ x: d.torque[0], y: d.torque[1], z: d.torque[2] }, true);
    } else if (d.type === 'seek') {
      const cur = rb.translation();
      const vel = rb.linvel();
      const dx = d.target[0] - cur.x;
      const dy = d.target[1] - cur.y;
      const dz = d.target[2] - cur.z;
      const dist = Math.hypot(dx, dy, dz);
      if (dist > 0.05) {
        const inv = d.speed / dist;
        const desiredVx = dx * inv;
        const desiredVy = dy * inv;
        const desiredVz = dz * inv;
        let fx = (desiredVx - vel.x) * 4.0;
        let fy = (desiredVy - vel.y) * 4.0;
        let fz = (desiredVz - vel.z) * 4.0;
        const fLen = Math.hypot(fx, fy, fz);
        if (fLen > d.maxForce && d.maxForce > 0) {
          const s = d.maxForce / fLen;
          fx *= s;
          fy *= s;
          fz *= s;
        }
        rb.resetForces(true);
        rb.addForce({ x: fx, y: fy, z: fz }, true);
      }
    } else if (d.type === 'flight') {
      const r = rb.rotation();
      const fwd = quatRotateVec(r, 0, 0, -1);
      const right = quatRotateVec(r, 1, 0, 0);

      rb.setLinvel(
        { x: fwd.x * d.speed, y: fwd.y * d.speed, z: fwd.z * d.speed },
        true
      );

      let pitchRate = d.pitch;
      if (d.autoLevel) {
        if (fwd.y > 0.82 && pitchRate > 0) pitchRate = 0;
        if (fwd.y < -0.82 && pitchRate < 0) pitchRate = 0;
      }

      let ax = right.x * pitchRate;
      let ay = right.y * pitchRate;
      let az = right.z * pitchRate;

      if (d.autoLevel) {
        ay += d.yaw;
        const targetBank = clamp(d.yaw * 0.30, -0.55, 0.55);
        const bankRate = (targetBank - right.y) * 4.5 + d.roll;
        ax += -fwd.x * bankRate;
        ay += -fwd.y * bankRate;
        az += -fwd.z * bankRate;
      } else {
        const up = quatRotateVec(r, 0, 1, 0);
        ax += up.x * d.yaw - fwd.x * d.roll;
        ay += up.y * d.yaw - fwd.y * d.roll;
        az += up.z * d.yaw - fwd.z * d.roll;
      }
      rb.setAngvel({ x: ax, y: ay, z: az }, true);
    }
  }
}

function approxEqArr(a, b) {
  for (let i = 0; i < a.length; i++) {
    if (Math.abs(a[i] - b[i]) > SNAP_EPS) return false;
  }
  return true;
}

function captureSnapshotDelta(forceAll) {
  const delta = {};
  for (const [id, entry] of bodies.entries()) {
    const kind = entry.spec.kind.type;
    if (kind === 'static' || kind === 'sensor' || !entry.rb) continue;

    const t = entry.rb.translation();
    const r = entry.rb.rotation();
    const lv = entry.rb.linvel();
    const av = entry.rb.angvel();
    const cur = {
      pos: [t.x, t.y, t.z],
      rot: [r.x, r.y, r.z, r.w],
      vel: [lv.x, lv.y, lv.z],
      angVel: [av.x, av.y, av.z],
    };

    const prev = lastSentSnap.get(id);
    if (
      !forceAll &&
      prev &&
      approxEqArr(prev.pos, cur.pos) &&
      approxEqArr(prev.rot, cur.rot) &&
      approxEqArr(prev.vel, cur.vel) &&
      approxEqArr(prev.angVel, cur.angVel)
    ) {
      continue;
    }

    lastSentSnap.set(id, cur);
    delta[String(id)] = cur;
  }
  return delta;
}

function takeFullSnapshot() {
  if (!physicsWorld) return '';
  const bytes = physicsWorld.takeSnapshot();
  const meta = {
    simTime,
    rapier: Buffer.from(bytes).toString('base64'),
    bodies: Array.from(bodies.values()).map((e) => ({
      id: e.id,
      spec: e.spec,
      rbHandle: e.rbHandle,
      colliderHandles: e.colliderHandles,
    })),
  };
  return Buffer.from(JSON.stringify(meta), 'utf8').toString('base64');
}

function restoreFullSnapshot(dataB64) {
  const meta = JSON.parse(Buffer.from(dataB64, 'base64').toString('utf8'));
  const rapierBytes = new Uint8Array(Buffer.from(meta.rapier, 'base64'));

  if (eventQueue) {
    eventQueue.free();
    eventQueue = null;
  }
  if (physicsWorld) {
    physicsWorld.free();
    physicsWorld = null;
  }

  physicsWorld = RAPIER.World.restoreSnapshot(rapierBytes);
  eventQueue = new RAPIER.EventQueue(true);
  simTime = meta.simTime || 0;

  bodies.clear();
  colliderHandleToId.clear();
  lastSentSnap.clear();

  for (const item of meta.bodies || []) {
    const rb = item.rbHandle !== null ? physicsWorld.getRigidBody(item.rbHandle) : null;
    const colliders = [];
    const colliderHandles = item.colliderHandles || [];
    for (const h of colliderHandles) {
      const col = physicsWorld.getCollider(h);
      if (col) colliders.push(col);
      colliderHandleToId.set(h, item.id);
    }
    bodies.set(item.id, {
      id: item.id,
      spec: item.spec,
      rb,
      rbHandle: item.rbHandle,
      colliders,
      colliderHandles,
    });
  }
}

function writeMsg(obj) {
  process.stdout.write(JSON.stringify(obj) + '\n');
}

function handleMessage(msg) {
  switch (msg.type) {
    case 'reset': {
      resetWorld(msg.gravity || [0, -22, 0]);
      const sortedBodies = (msg.bodies || []).slice().sort((a, b) => a.id - b.id);
      for (const spec of sortedBodies) {
        spawnBody(spec);
      }
      executeCommands(msg.commands);
      writeMsg({ type: 'ok', snapshotDelta: captureSnapshotDelta(true) });
      break;
    }
    case 'delta': {
      if (msg.gravity !== undefined && physicsWorld) {
        physicsWorld.gravity = {
          x: msg.gravity[0],
          y: msg.gravity[1],
          z: msg.gravity[2],
        };
      }
      if (msg.removed) {
        for (const id of msg.removed) removeBody(id);
      }
      if (msg.added) {
        const sortedAdd = msg.added.slice().sort((a, b) => a.id - b.id);
        for (const spec of sortedAdd) spawnBody(spec);
      }
      if (msg.updated) {
        for (const u of msg.updated) applyBodyUpdate(u);
      }
      executeCommands(msg.commands);
      writeMsg({ type: 'ok' });
      break;
    }
    case 'step': {
      const dt = typeof msg.dt === 'number' ? msg.dt : FIXED_DT;
      if (!physicsWorld || !eventQueue) {
        writeMsg({ type: 'stepped', time: simTime, collisions: [], snapshotDelta: {} });
        break;
      }
      simTime += dt;
      applyDrivesAndKinematics(dt);
      physicsWorld.step(eventQueue);

      const seen = new Set();
      const collisions = [];
      eventQueue.drainCollisionEvents((h1, h2, started) => {
        const idA = colliderHandleToId.get(h1);
        const idB = colliderHandleToId.get(h2);
        if (idA !== undefined && idB !== undefined && idA !== idB) {
          const a = Math.min(idA, idB);
          const b = Math.max(idA, idB);
          const type = started ? 'collisionStart' : 'collisionEnd';
          const key = `${type}:${a}:${b}`;
          if (!seen.has(key)) {
            seen.add(key);
            collisions.push({ type, a, b });
          }
        }
      });

      collisions.sort((x, y) => {
        if (x.type !== y.type) return x.type < y.type ? -1 : 1;
        if (x.a !== y.a) return x.a - y.a;
        return x.b - y.b;
      });

      writeMsg({
        type: 'stepped',
        time: simTime,
        collisions,
        snapshotDelta: captureSnapshotDelta(false),
      });
      break;
    }
    case 'takeSnapshot': {
      writeMsg({ type: 'snapshot', data: takeFullSnapshot() });
      break;
    }
    case 'restoreSnapshot': {
      restoreFullSnapshot(msg.data);
      writeMsg({ type: 'ok', snapshotDelta: captureSnapshotDelta(true) });
      break;
    }
    case 'quit': {
      if (eventQueue) eventQueue.free();
      if (physicsWorld) physicsWorld.free();
      process.exit(0);
      break;
    }
    default:
      writeMsg({ type: 'ok' });
      break;
  }
}

async function main() {
  await RAPIER.init();
  writeMsg({ type: 'ready' });

  const rl = readline.createInterface({
    input: process.stdin,
    crlfDelay: Infinity,
  });

  rl.on('line', (line) => {
    const trimmed = line.trim();
    if (!trimmed) return;
    try {
      const msg = JSON.parse(trimmed);
      handleMessage(msg);
    } catch (err) {
      writeMsg({ type: 'error', message: String(err) });
    }
  });

  rl.on('close', () => {
    if (eventQueue) eventQueue.free();
    if (physicsWorld) physicsWorld.free();
    process.exit(0);
  });
}

main().catch((err) => {
  process.stderr.write(`headless-physics fatal: ${err.stack || err}\n`);
  process.exit(1);
});

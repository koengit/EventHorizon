import * as THREE from 'three';
import RAPIER from '@dimforge/rapier3d-compat';

const FIXED_DT = 1 / 60;
const MAX_SUBSTEPS = 4;
const SNAP_EPS = 1e-4;

const canvas = document.getElementById('viewport');
const hudRoot = document.getElementById('hud');
const statusBadge = document.getElementById('status-badge');

const renderer = new THREE.WebGLRenderer({ canvas, antialias: true });
renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.shadowMap.enabled = true;
renderer.shadowMap.type = THREE.PCFSoftShadowMap;
renderer.toneMapping = THREE.ACESFilmicToneMapping;
renderer.toneMappingExposure = 1.05;

const threeScene = new THREE.Scene();
threeScene.background = new THREE.Color(0.48, 0.76, 0.96);

const camera = new THREE.PerspectiveCamera(
  60,
  window.innerWidth / window.innerHeight,
  0.1,
  900
);
camera.position.set(0, 10, 18);

const lightsGroup = new THREE.Group();
const bodiesGroup = new THREE.Group();
threeScene.add(lightsGroup);
threeScene.add(bodiesGroup);

window.addEventListener('resize', () => {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
});

// Shared GPU geometry & material caches (prevents duplicate GPU allocations
// when scenes contain hundreds/thousands of repeated primitives).
const geometryCache = new Map();
const materialCache = new Map();

function getCachedGeometry(spec) {
  let key;
  switch (spec.type) {
    case 'box':
      key = `box:${spec.size[0]}:${spec.size[1]}:${spec.size[2]}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(
          key,
          new THREE.BoxGeometry(spec.size[0], spec.size[1], spec.size[2])
        );
      }
      return geometryCache.get(key);
    case 'sphere':
      key = `sphere:${spec.radius}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(key, new THREE.SphereGeometry(spec.radius, 20, 14));
      }
      return geometryCache.get(key);
    case 'cylinder':
      key = `cyl:${spec.radiusTop}:${spec.radiusBottom}:${spec.height}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(
          key,
          new THREE.CylinderGeometry(
            spec.radiusTop,
            spec.radiusBottom,
            spec.height,
            18
          )
        );
      }
      return geometryCache.get(key);
    case 'cone':
      key = `cone:${spec.radius}:${spec.height}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(
          key,
          new THREE.ConeGeometry(spec.radius, spec.height, 18)
        );
      }
      return geometryCache.get(key);
    case 'capsule':
      key = `cap:${spec.radius}:${spec.height}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(
          key,
          new THREE.CapsuleGeometry(spec.radius, spec.height, 6, 14)
        );
      }
      return geometryCache.get(key);
    case 'torus':
      key = `torus:${spec.radius}:${spec.tube}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(
          key,
          new THREE.TorusGeometry(spec.radius, spec.tube, 12, 24)
        );
      }
      return geometryCache.get(key);
    case 'plane':
      key = `plane:${spec.width}:${spec.height}`;
      if (!geometryCache.has(key)) {
        geometryCache.set(key, new THREE.PlaneGeometry(spec.width, spec.height));
      }
      return geometryCache.get(key);
    default:
      return null;
  }
}

function getCachedMaterial(style) {
  const col = style.color || [0.8, 0.8, 0.82];
  const em = style.emissive;
  const key = [
    col[0].toFixed(3),
    col[1].toFixed(3),
    col[2].toFixed(3),
    em
      ? `${em.color[0].toFixed(3)},${em.color[1].toFixed(3)},${em.color[2].toFixed(3)},${em.intensity.toFixed(2)}`
      : 'none',
    style.roughness.toFixed(2),
    style.metalness.toFixed(2),
    style.opacity.toFixed(2),
    style.wireframe ? '1' : '0',
  ].join('|');

  if (!materialCache.has(key)) {
    const params = {
      color: new THREE.Color(col[0], col[1], col[2]),
      roughness: style.roughness,
      metalness: style.metalness,
      wireframe: style.wireframe,
    };
    if (style.opacity < 0.999) {
      params.transparent = true;
      params.opacity = style.opacity;
    }
    if (em) {
      params.emissive = new THREE.Color(em.color[0], em.color[1], em.color[2]);
      params.emissiveIntensity = em.intensity;
    }
    materialCache.set(key, new THREE.MeshStandardMaterial(params));
  }
  return materialCache.get(key);
}

const DEFAULT_STYLE = {
  color: null,
  emissive: null,
  roughness: 0.62,
  metalness: 0.08,
  opacity: 1.0,
  wireframe: false,
  castShadow: true,
  receiveShadow: true,
};

function makeColor(c) {
  return new THREE.Color(c[0], c[1], c[2]);
}

function buildMesh(spec, style = DEFAULT_STYLE) {
  switch (spec.type) {
    case 'empty':
      return new THREE.Group();
    case 'color':
      return buildMesh(spec.child, { ...style, color: spec.color });
    case 'emissive':
      return buildMesh(spec.child, {
        ...style,
        emissive: { color: spec.color, intensity: spec.intensity },
      });
    case 'material': {
      const m = spec.material;
      return buildMesh(spec.child, {
        color: m.color || style.color,
        emissive: m.emissive || style.emissive,
        roughness: m.roughness,
        metalness: m.metalness,
        opacity: m.opacity,
        wireframe: m.wireframe,
        castShadow: m.castShadow,
        receiveShadow: m.receiveShadow,
      });
    }
    case 'translate': {
      const g = new THREE.Group();
      g.position.set(spec.offset[0], spec.offset[1], spec.offset[2]);
      g.add(buildMesh(spec.child, style));
      return g;
    }
    case 'rotate': {
      const g = new THREE.Group();
      g.rotation.set(spec.euler[0], spec.euler[1], spec.euler[2]);
      g.add(buildMesh(spec.child, style));
      return g;
    }
    case 'scale': {
      const g = new THREE.Group();
      g.scale.set(spec.factor[0], spec.factor[1], spec.factor[2]);
      g.add(buildMesh(spec.child, style));
      return g;
    }
    case 'group': {
      const g = new THREE.Group();
      for (const ch of spec.children) {
        g.add(buildMesh(ch, style));
      }
      return g;
    }
    default: {
      const geom = getCachedGeometry(spec);
      if (!geom) return new THREE.Group();
      const mat = getCachedMaterial(style);
      const mesh = new THREE.Mesh(geom, mat);
      mesh.castShadow = style.castShadow;
      mesh.receiveShadow = style.receiveShadow;
      return mesh;
    }
  }
}

// Procedural Web Audio API synthesizer (zero external assets required).
let audioCtx = null;
let noiseBuffer = null;

function ensureAudio() {
  if (!audioCtx) {
    const Ctx = window.AudioContext || window.webkitAudioContext;
    if (!Ctx) return null;
    audioCtx = new Ctx();
    const len = Math.floor(audioCtx.sampleRate * 1.0);
    noiseBuffer = audioCtx.createBuffer(1, len, audioCtx.sampleRate);
    const data = noiseBuffer.getChannelData(0);
    for (let i = 0; i < len; i++) {
      data[i] = Math.random() * 2 - 1;
    }
  }
  if (audioCtx.state === 'suspended') {
    audioCtx.resume().catch(() => {});
  }
  return audioCtx;
}

function playSound(snd) {
  const ctx = ensureAudio();
  if (!ctx) return;
  const now = ctx.currentTime;
  const dur = Math.max(0.02, snd.duration || 0.15);
  const vol = Math.max(0.001, Math.min(1.0, snd.volume || 0.2));

  const gain = ctx.createGain();
  gain.gain.setValueAtTime(vol, now);
  gain.gain.exponentialRampToValueAtTime(0.0008, now + dur);
  gain.connect(ctx.destination);

  if (snd.type === 'tone') {
    const osc = ctx.createOscillator();
    osc.type = snd.wave || 'sine';
    const f0 = Math.max(20, snd.freqStart || 440);
    const f1 = Math.max(20, snd.freqEnd || f0);
    osc.frequency.setValueAtTime(f0, now);
    if (Math.abs(f1 - f0) > 1) {
      osc.frequency.exponentialRampToValueAtTime(f1, now + dur);
    }
    osc.connect(gain);
    osc.start(now);
    osc.stop(now + dur + 0.01);
  } else if (snd.type === 'noise' && noiseBuffer) {
    const src = ctx.createBufferSource();
    src.buffer = noiseBuffer;
    const filter = ctx.createBiquadFilter();
    filter.type = 'lowpass';
    filter.frequency.setValueAtTime(1400, now);
    filter.frequency.exponentialRampToValueAtTime(180, now + dur);
    src.connect(filter);
    filter.connect(gain);
    src.start(now);
    src.stop(now + dur + 0.01);
  }
}

let physicsWorld = null;
let eventQueue = null;
let currentEpoch = -1;
let simTime = 0;
let runtimeMode = 'single'; // 'single' | 'authoritative' | 'predict'
let localTick = 0;
let cameraSpec = null;
let cameraInitialized = false;
let currentLookAt = new THREE.Vector3(0, 0, 0);
let primaryDirLight = null;
let primaryDirLightOffset = new THREE.Vector3(25, 45, 20);

const bodies = new Map(); // wireId (number) -> { id, spec, rb, rbHandle, colliders, colliderHandles, meshObj, prevPhysPos, prevPhysQuat, physPos, physQuat, errPos, errQuat }
const colliderHandleToId = new Map(); // handle -> wireId
const activeTimers = new Map(); // wireId -> { id, mode, interval, remaining, fired }
const lastSentSnap = new Map(); // wireId -> { pos, rot, vel, angVel }
const heldKeys = new Set(); // key codes currently held (cleared on blur)
const rapierSnapshots = new Map(); // tick -> { simTime, bytes, bodiesMeta }
const MAX_RAPIER_SNAPSHOTS = 180;
const IDENTITY_QUAT = new THREE.Quaternion(0, 0, 0, 1);
const MAX_SMOOTH_DIST_SQ = 15 * 15;
const authPoseQueue = []; // authoritative mode 60 Hz pose frames for smooth interpolation
let authInterpAcc = 0;
let authHasFirstPose = false;

let ws = null;
let haskellEventCount = 0;
let lastPatchBytes = 0;
let wsConnected = false;

function updateStatusBadge() {
  if (!wsConnected) {
    statusBadge.textContent = 'Disconnected (reload to reconnect)';
  } else {
    statusBadge.textContent =
      `EventHorizon · Events: ${haskellEventCount} · Bodies: ${bodies.size} · Last patch: ${lastPatchBytes} B`;
  }
}

function resetWorld(gravityArr) {
  for (const entry of bodies.values()) {
    bodiesGroup.remove(entry.meshObj);
  }
  bodies.clear();
  colliderHandleToId.clear();
  activeTimers.clear();
  lastSentSnap.clear();
  rapierSnapshots.clear();
  authPoseQueue.length = 0;
  authInterpAcc = 0;
  authHasFirstPose = false;
  localTick = 0;
  cameraInitialized = false;

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

function tagMeshWithBodyId(obj, id) {
  obj.userData.bodyId = id;
  obj.traverse((child) => {
    child.userData.bodyId = id;
  });
}

function spawnBody(spec) {
  if (bodies.has(spec.id)) {
    removeBody(spec.id);
  }

  const meshObj = buildMesh(spec.mesh);
  meshObj.position.set(spec.pose.pos[0], spec.pose.pos[1], spec.pose.pos[2]);
  meshObj.quaternion.set(
    spec.pose.rot[0],
    spec.pose.rot[1],
    spec.pose.rot[2],
    spec.pose.rot[3]
  );
  tagMeshWithBodyId(meshObj, spec.id);
  bodiesGroup.add(meshObj);

  let rb = null;
  let rbHandle = null;
  const colliders = [];
  const colliderHandles = [];
  const colDescs = runtimeMode === 'authoritative' ? [] : collectColliderDescs(spec.collider);

  if (colDescs.length > 0 && physicsWorld) {
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
    spec,
    rb,
    rbHandle,
    colliders,
    colliderHandles,
    meshObj,
    prevPhysPos: new THREE.Vector3(spec.pose.pos[0], spec.pose.pos[1], spec.pose.pos[2]),
    prevPhysQuat: new THREE.Quaternion(
      spec.pose.rot[0],
      spec.pose.rot[1],
      spec.pose.rot[2],
      spec.pose.rot[3]
    ),
    physPos: new THREE.Vector3(spec.pose.pos[0], spec.pose.pos[1], spec.pose.pos[2]),
    physQuat: new THREE.Quaternion(
      spec.pose.rot[0],
      spec.pose.rot[1],
      spec.pose.rot[2],
      spec.pose.rot[3]
    ),
    errPos: new THREE.Vector3(0, 0, 0),
    errQuat: new THREE.Quaternion(0, 0, 0, 1),
  });
}

function removeBody(id) {
  const entry = bodies.get(id);
  if (!entry) return;
  bodiesGroup.remove(entry.meshObj);
  for (const col of entry.colliders) {
    colliderHandleToId.delete(col.handle);
  }
  if (entry.rb) {
    physicsWorld.removeRigidBody(entry.rb);
  }
  bodies.delete(id);
  lastSentSnap.delete(id);
}

function applyBodyUpdate(u) {
  const entry = bodies.get(u.id);
  if (!entry) return;
  const spec = entry.spec;

  if (u.mesh !== undefined) {
    spec.mesh = u.mesh;
    const pos = entry.meshObj.position.clone();
    const quat = entry.meshObj.quaternion.clone();
    bodiesGroup.remove(entry.meshObj);
    const replacement = buildMesh(u.mesh);
    replacement.position.copy(pos);
    replacement.quaternion.copy(quat);
    tagMeshWithBodyId(replacement, u.id);
    bodiesGroup.add(replacement);
    entry.meshObj = replacement;
  }

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
      entry.prevPhysPos.set(u.pose.pos[0], u.pose.pos[1], u.pose.pos[2]);
      entry.physPos.set(u.pose.pos[0], u.pose.pos[1], u.pose.pos[2]);
      entry.prevPhysQuat.set(
        u.pose.rot[0],
        u.pose.rot[1],
        u.pose.rot[2],
        u.pose.rot[3]
      );
      entry.physQuat.set(
        u.pose.rot[0],
        u.pose.rot[1],
        u.pose.rot[2],
        u.pose.rot[3]
      );
      entry.meshObj.position.set(u.pose.pos[0], u.pose.pos[1], u.pose.pos[2]);
      entry.meshObj.quaternion.set(
        u.pose.rot[0],
        u.pose.rot[1],
        u.pose.rot[2],
        u.pose.rot[3]
      );
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

function applyLights(lightsSpec) {
  while (lightsGroup.children.length > 0) {
    lightsGroup.remove(lightsGroup.children[0]);
  }
  primaryDirLight = null;

  for (const l of lightsSpec) {
    if (l.type === 'ambient') {
      lightsGroup.add(new THREE.AmbientLight(makeColor(l.color), l.intensity));
    } else if (l.type === 'hemisphere') {
      lightsGroup.add(
        new THREE.HemisphereLight(
          makeColor(l.skyColor),
          makeColor(l.groundColor),
          l.intensity
        )
      );
    } else if (l.type === 'directional') {
      const dl = new THREE.DirectionalLight(makeColor(l.color), l.intensity);
      dl.position.set(l.pos[0], l.pos[1], l.pos[2]);
      dl.castShadow = true;
      dl.shadow.mapSize.width = 2048;
      dl.shadow.mapSize.height = 2048;
      dl.shadow.camera.near = 1;
      dl.shadow.camera.far = 260;
      const extent = 75;
      dl.shadow.camera.left = -extent;
      dl.shadow.camera.right = extent;
      dl.shadow.camera.top = extent;
      dl.shadow.camera.bottom = -extent;
      dl.shadow.bias = -0.0005;
      lightsGroup.add(dl);
      lightsGroup.add(dl.target);
      if (!primaryDirLight) {
        primaryDirLight = dl;
        primaryDirLightOffset.set(l.pos[0], l.pos[1], l.pos[2]);
      }
    } else if (l.type === 'point') {
      const pl = new THREE.PointLight(
        makeColor(l.color),
        l.intensity,
        l.distance
      );
      pl.position.set(l.pos[0], l.pos[1], l.pos[2]);
      lightsGroup.add(pl);
    }
  }
}

function applyHud(hudSpec) {
  hudRoot.innerHTML = '';
  const groups = new Map();

  function getGroup(anchor) {
    if (!groups.has(anchor)) {
      const div = document.createElement('div');
      div.className = `hud-group hud-${anchor}`;
      hudRoot.appendChild(div);
      groups.set(anchor, div);
    }
    return groups.get(anchor);
  }

  for (const item of hudSpec) {
    if (item.type === 'text') {
      const pill = document.createElement('div');
      pill.className = 'hud-pill';
      pill.textContent = item.text;
      getGroup(item.anchor).appendChild(pill);
    } else if (item.type === 'crosshair') {
      const ch = document.createElement('div');
      ch.className = 'hud-crosshair';
      hudRoot.appendChild(ch);
    } else if (item.type === 'banner') {
      const b = document.createElement('div');
      b.className = 'hud-banner';
      const h1 = document.createElement('h1');
      h1.textContent = item.title;
      const p = document.createElement('p');
      p.textContent = item.subtitle;
      b.appendChild(h1);
      b.appendChild(p);
      hudRoot.appendChild(b);
    }
  }
}

function applyTimers(timersSpec) {
  const incomingIds = new Set(timersSpec.map((t) => t.id));
  for (const id of Array.from(activeTimers.keys())) {
    if (!incomingIds.has(id)) {
      activeTimers.delete(id);
    }
  }
  for (const t of timersSpec) {
    const interval = Math.max(0.01, t.seconds);
    const existing = activeTimers.get(t.id);
    if (!existing) {
      activeTimers.set(t.id, {
        id: t.id,
        mode: t.mode,
        interval,
        remaining: interval,
        fired: false,
      });
    } else if (existing.mode !== t.mode || Math.abs(existing.interval - interval) > 1e-6) {
      existing.mode = t.mode;
      existing.interval = interval;
      existing.remaining = Math.min(existing.remaining, interval);
    }
  }
}

function applyCameraSpec(spec) {
  cameraSpec = spec;
  if (spec && typeof spec.fov === 'number' && Math.abs(camera.fov - spec.fov) > 1e-3) {
    camera.fov = spec.fov;
    camera.updateProjectionMatrix();
  }
}

function applyFog(fog) {
  if (fog) {
    threeScene.fog = new THREE.Fog(makeColor(fog.color), fog.near, fog.far);
  } else {
    threeScene.fog = null;
  }
}

function executeCommands(commands, silent = false) {
  if (!commands || commands.length === 0) return;
  for (const cmd of commands) {
    if (cmd.type === 'sound') {
      if (!silent) playSound(cmd.sound);
      continue;
    }
    const entry = bodies.get(cmd.id);
    if (!entry) continue;
    if (cmd.type === 'impulse' && entry.rb) {
      entry.rb.applyImpulse(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'torqueImpulse' && entry.rb) {
      entry.rb.applyTorqueImpulse(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'setVel' && entry.rb) {
      entry.rb.setLinvel(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'setAngVel' && entry.rb) {
      entry.rb.setAngvel(
        { x: cmd.vec[0], y: cmd.vec[1], z: cmd.vec[2] },
        true
      );
    } else if (cmd.type === 'teleport') {
      const p = cmd.pose.pos;
      const r = cmd.pose.rot;
      if (entry.rb) {
        entry.rb.setTranslation({ x: p[0], y: p[1], z: p[2] }, true);
        entry.rb.setRotation({ x: r[0], y: r[1], z: r[2], w: r[3] }, true);
      }
      entry.prevPhysPos.set(p[0], p[1], p[2]);
      entry.prevPhysQuat.set(r[0], r[1], r[2], r[3]);
      entry.physPos.set(p[0], p[1], p[2]);
      entry.physQuat.set(r[0], r[1], r[2], r[3]);
      entry.errPos.set(0, 0, 0);
      entry.errQuat.copy(IDENTITY_QUAT);
      entry.meshObj.position.set(p[0], p[1], p[2]);
      entry.meshObj.quaternion.set(r[0], r[1], r[2], r[3]);
    }
  }
}

function applyReset(msg) {
  currentEpoch = msg.epoch;
  resetWorld(msg.gravity);
  threeScene.background = makeColor(msg.background);
  applyFog(msg.fog);
  applyLights(msg.lights);
  applyHud(msg.hud);
  applyTimers(msg.timers);
  applyCameraSpec(msg.camera);
  for (const spec of msg.bodies) {
    spawnBody(spec);
  }
  executeCommands(msg.commands);
  updateStatusBadge();
}

function decodeBase64Bytes(b64) {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) {
    bytes[i] = bin.charCodeAt(i);
  }
  return bytes;
}

function takeRapierSnapshot(tick) {
  if (!physicsWorld || runtimeMode === 'authoritative') return;
  const bytes = physicsWorld.takeSnapshot();
  const bodiesMeta = [];
  for (const entry of bodies.values()) {
    bodiesMeta.push({
      id: entry.id,
      rbHandle: entry.rbHandle,
      colliderHandles: entry.colliderHandles.slice(),
    });
  }
  rapierSnapshots.set(tick, { simTime, bytes, bodiesMeta });
  if (rapierSnapshots.size > MAX_RAPIER_SNAPSHOTS) {
    const oldestTick = rapierSnapshots.keys().next().value;
    rapierSnapshots.delete(oldestTick);
  }
}

function captureVisualErrorOrigin() {
  const saved = new Map();
  for (const [id, entry] of bodies.entries()) {
    saved.set(id, {
      pos: entry.meshObj.position.clone(),
      quat: entry.meshObj.quaternion.clone(),
    });
  }
  return saved;
}

function applyVisualSmoothingFrom(savedVisuals) {
  for (const [id, entry] of bodies.entries()) {
    const prev = savedVisuals.get(id);
    if (!prev) continue;
    if (entry.rb) {
      const t = entry.rb.translation();
      const r = entry.rb.rotation();
      entry.physPos.set(t.x, t.y, t.z);
      entry.physQuat.set(r.x, r.y, r.z, r.w);
      entry.prevPhysPos.copy(entry.physPos);
      entry.prevPhysQuat.copy(entry.physQuat);
    }
    const diffPos = tmpVecA.copy(prev.pos).sub(entry.physPos);
    if (diffPos.lengthSq() <= MAX_SMOOTH_DIST_SQ) {
      entry.errPos.copy(diffPos);
      entry.errQuat.copy(prev.quat).multiply(tmpQuatA.copy(entry.physQuat).invert());
    } else {
      entry.errPos.set(0, 0, 0);
      entry.errQuat.copy(IDENTITY_QUAT);
    }
  }
}

function restoreRapierSnapshot(tick, snapBase64 = null) {
  let bytes = null;
  let bodiesMeta = null;
  if (snapBase64) {
    bytes = decodeBase64Bytes(snapBase64);
  } else {
    const snap = rapierSnapshots.get(tick);
    if (!snap) return false;
    bytes = snap.bytes;
    bodiesMeta = snap.bodiesMeta;
    simTime = snap.simTime;
  }

  if (eventQueue) {
    eventQueue.free();
    eventQueue = null;
  }
  if (physicsWorld) {
    physicsWorld.free();
    physicsWorld = null;
  }

  physicsWorld = RAPIER.World.restoreSnapshot(bytes);
  eventQueue = new RAPIER.EventQueue(true);
  colliderHandleToId.clear();

  if (bodiesMeta) {
    for (const meta of bodiesMeta) {
      const entry = bodies.get(meta.id);
      if (!entry) continue;
      entry.rb = meta.rbHandle !== null ? physicsWorld.getRigidBody(meta.rbHandle) : null;
      entry.rbHandle = meta.rbHandle;
      entry.colliders = [];
      entry.colliderHandles = meta.colliderHandles.slice();
      for (const h of entry.colliderHandles) {
        const col = physicsWorld.getCollider(h);
        if (col) entry.colliders.push(col);
        colliderHandleToId.set(h, entry.id);
      }
    }
  } else {
    for (const entry of bodies.values()) {
      if (entry.rbHandle !== null) {
        entry.rb = physicsWorld.getRigidBody(entry.rbHandle);
      }
      entry.colliders = [];
      for (const h of entry.colliderHandles) {
        const col = physicsWorld.getCollider(h);
        if (col) entry.colliders.push(col);
        colliderHandleToId.set(h, entry.id);
      }
    }
  }
  return true;
}

function applyReplaySteps(msg) {
  const preVisuals = captureVisualErrorOrigin();
  if (msg.restoreTick !== undefined || msg.snapBase64) {
    restoreRapierSnapshot(msg.restoreTick, msg.snapBase64 || null);
  }
  for (const step of msg.steps || []) {
    if (step.tick !== undefined) {
      takeRapierSnapshot(step.tick);
    }
    if (step.delta) {
      applyDelta(step.delta, true);
    }
    if (step.commands) {
      executeCommands(step.commands, true);
    }
    if (physicsWorld && eventQueue && runtimeMode !== 'authoritative') {
      const dt = typeof step.dt === 'number' ? step.dt : FIXED_DT;
      simTime += dt;
      applyDrivesAndKinematics(dt);
      physicsWorld.step(eventQueue);
      eventQueue.drainCollisionEvents(() => {});
    }
  }
  applyVisualSmoothingFrom(preVisuals);
}

function applyAuthoritativePoseFrame(frameMsg, snapImmediate = false) {
  const smooth = !!frameMsg.smooth;
  const poseMap = frameMsg.poses || {};

  // Shift every body's previous segment end into its new segment start so
  // stationary bodies omitted from delta-encoded pose frames stay still.
  for (const entry of bodies.values()) {
    entry.prevPhysPos.copy(entry.physPos);
    entry.prevPhysQuat.copy(entry.physQuat);
  }

  for (const [idStr, p] of Object.entries(poseMap)) {
    const id = Number(idStr);
    const entry = bodies.get(id);
    if (!entry) continue;
    const prevVisPos = tmpVecB.copy(entry.meshObj.position);
    const prevVisQuat = tmpQuatB.copy(entry.meshObj.quaternion);
    entry.physPos.set(p.pos[0], p.pos[1], p.pos[2]);
    entry.physQuat.set(p.rot[0], p.rot[1], p.rot[2], p.rot[3]);

    // Never smear across large teleports or initial pose snap.
    const stepDistSq = tmpVecA.copy(entry.physPos).sub(entry.prevPhysPos).lengthSq();
    if (snapImmediate || stepDistSq > MAX_SMOOTH_DIST_SQ) {
      entry.prevPhysPos.copy(entry.physPos);
      entry.prevPhysQuat.copy(entry.physQuat);
    }

    if (smooth) {
      entry.prevPhysPos.copy(entry.physPos);
      entry.prevPhysQuat.copy(entry.physQuat);
      const diffPos = tmpVecA.copy(prevVisPos).sub(entry.physPos);
      if (diffPos.lengthSq() <= MAX_SMOOTH_DIST_SQ) {
        entry.errPos.copy(diffPos);
        entry.errQuat.copy(prevVisQuat).multiply(tmpQuatA.copy(entry.physQuat).invert());
      } else {
        entry.errPos.set(0, 0, 0);
        entry.errQuat.copy(IDENTITY_QUAT);
      }
    }
  }
}

function applyPoses(msg) {
  if (!authHasFirstPose) {
    authHasFirstPose = true;
    authInterpAcc = 0;
    applyAuthoritativePoseFrame(msg, true);
    return;
  }
  authPoseQueue.push(msg);
  // Cap latency if browser tab was backgrounded and accumulated a backlog.
  while (authPoseQueue.length > 6) {
    applyAuthoritativePoseFrame(authPoseQueue.shift(), false);
  }
}

function advanceAuthoritativeInterpolation(dt) {
  if (authPoseQueue.length === 0) {
    authInterpAcc = Math.min(FIXED_DT, authInterpAcc + dt);
    return Math.max(0, Math.min(1, authInterpAcc / FIXED_DT));
  }

  // If the queue had momentarily run dry (holding at 1.0), start the newly
  // arrived segment cleanly from 0 instead of snapping straight through it.
  if (authInterpAcc >= FIXED_DT) {
    applyAuthoritativePoseFrame(authPoseQueue.shift(), false);
    authInterpAcc = 0;
  }

  const qLen = authPoseQueue.length;
  // Adaptive playback speed keeps ~1 tick (16.7 ms) of jitter cushion so
  // WebSocket arrival jitter never stalls or double-jumps a rendered frame.
  let rate = 1.0;
  if (qLen === 2) {
    rate = 1.12;
  } else if (qLen >= 3) {
    rate = 1.35;
  }

  authInterpAcc += dt * rate;
  while (authInterpAcc >= FIXED_DT && authPoseQueue.length > 0) {
    applyAuthoritativePoseFrame(authPoseQueue.shift(), false);
    authInterpAcc -= FIXED_DT;
  }
  if (authInterpAcc > FIXED_DT) {
    authInterpAcc = FIXED_DT;
  }

  return Math.max(0, Math.min(1, authInterpAcc / FIXED_DT));
}

function applyDelta(msg, silent = false) {
  if (msg.epoch !== currentEpoch) return;
  if (msg.gravity !== undefined && physicsWorld) {
    physicsWorld.gravity = {
      x: msg.gravity[0],
      y: msg.gravity[1],
      z: msg.gravity[2],
    };
  }
  if (msg.background !== undefined) {
    threeScene.background = makeColor(msg.background);
  }
  if (msg.fog !== undefined) {
    applyFog(msg.fog);
  }
  if (msg.lights !== undefined) {
    applyLights(msg.lights);
  }
  if (msg.hud !== undefined) {
    applyHud(msg.hud);
  }
  if (msg.timers !== undefined) {
    applyTimers(msg.timers);
  }
  if (msg.camera !== undefined) {
    applyCameraSpec(msg.camera);
  }
  if (msg.removed) {
    for (const id of msg.removed) {
      removeBody(id);
    }
  }
  if (msg.added) {
    for (const spec of msg.added) {
      spawnBody(spec);
    }
  }
  if (msg.updated) {
    for (const u of msg.updated) {
      applyBodyUpdate(u);
    }
  }
  executeCommands(msg.commands, silent);
  updateStatusBadge();
}

function approxEqArr(a, b) {
  for (let i = 0; i < a.length; i++) {
    if (Math.abs(a[i] - b[i]) > SNAP_EPS) return false;
  }
  return true;
}

// Delta snapshot: skips static/sensor bodies completely and only transmits
// dynamic/kinematic bodies whose state actually changed since last send.
function captureSnapshotDelta() {
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

function sendEvents(events) {
  if (!ws || ws.readyState !== WebSocket.OPEN || events.length === 0) return;
  haskellEventCount += events.length;
  updateStatusBadge();
  ws.send(
    JSON.stringify({
      epoch: currentEpoch,
      time: simTime,
      events,
      snapshotDelta: captureSnapshotDelta(),
    })
  );
}

const tmpVecA = new THREE.Vector3();
const tmpVecB = new THREE.Vector3();
const tmpVecC = new THREE.Vector3();
const tmpQuatA = new THREE.Quaternion();
const tmpQuatB = new THREE.Quaternion();
const tmpInterpPos = new THREE.Vector3();
const tmpInterpQuat = new THREE.Quaternion();

function applyDrivesAndKinematics(dt) {
  for (const entry of bodies.values()) {
    const { spec, rb } = entry;
    if (!rb) continue;

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
      tmpQuatA.set(cr.x, cr.y, cr.z, cr.w);
      tmpQuatB.set(
        spec.pose.rot[0],
        spec.pose.rot[1],
        spec.pose.rot[2],
        spec.pose.rot[3]
      );
      tmpQuatA.slerp(tmpQuatB, Math.min(1, dt * 8));
      rb.setNextKinematicRotation({
        x: tmpQuatA.x,
        y: tmpQuatA.y,
        z: tmpQuatA.z,
        w: tmpQuatA.w,
      });
      continue;
    }

    if (spec.kind.type !== 'dynamic') continue;
    const d = spec.drive;
    if (!d || d.type === 'none') continue;

    if (d.type === 'walk') {
      const curVel = rb.linvel();
      rb.setLinvel({ x: d.vel[0], y: curVel.y, z: d.vel[2] }, true);
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
      tmpQuatA.set(r.x, r.y, r.z, r.w);
      const fwd = tmpVecA.set(0, 0, -1).applyQuaternion(tmpQuatA);
      const right = tmpVecB.set(1, 0, 0).applyQuaternion(tmpQuatA);

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
        const targetBank = THREE.MathUtils.clamp(d.yaw * 0.30, -0.55, 0.55);
        const bankRate = (targetBank - right.y) * 4.5 + d.roll;
        ax += -fwd.x * bankRate;
        ay += -fwd.y * bankRate;
        az += -fwd.z * bankRate;
      } else {
        const up = tmpVecC.set(0, 1, 0).applyQuaternion(tmpQuatA);
        ax += up.x * d.yaw - fwd.x * d.roll;
        ay += up.y * d.yaw - fwd.y * d.roll;
        az += up.z * d.yaw - fwd.z * d.roll;
      }
      rb.setAngvel({ x: ax, y: ay, z: az }, true);
    }
  }
}

function stepPhysics(dt) {
  if (!physicsWorld || !eventQueue) return;

  simTime += dt;
  applyDrivesAndKinematics(dt);
  physicsWorld.step(eventQueue);

  const pendingEvents = [];
  eventQueue.drainCollisionEvents((h1, h2, started) => {
    const idA = colliderHandleToId.get(h1);
    const idB = colliderHandleToId.get(h2);
    if (idA !== undefined && idB !== undefined && idA !== idB) {
      pendingEvents.push({
        type: started ? 'collisionStart' : 'collisionEnd',
        a: idA,
        b: idB,
      });
    }
  });

  for (const timer of activeTimers.values()) {
    if (timer.mode === 'once' && timer.fired) continue;
    timer.remaining -= dt;
    if (timer.remaining <= 0) {
      if (timer.mode === 'once') {
        timer.fired = true;
      } else {
        timer.remaining += timer.interval;
      }
      pendingEvents.push({ type: 'timer', id: timer.id });
    }
  }

  if (pendingEvents.length > 0) {
    sendEvents(pendingEvents);
  }
}

function snapshotPrevPhysicsPoses() {
  for (const entry of bodies.values()) {
    if (entry.rb) {
      entry.prevPhysPos.copy(entry.physPos);
      entry.prevPhysQuat.copy(entry.physQuat);
    }
  }
}

function readCurrentPhysicsPoses() {
  for (const entry of bodies.values()) {
    if (entry.rb) {
      const t = entry.rb.translation();
      const r = entry.rb.rotation();
      entry.physPos.set(t.x, t.y, t.z);
      entry.physQuat.set(r.x, r.y, r.z, r.w);
    }
  }
}

function syncVisuals(dt = FIXED_DT, interpAlpha = 1.0) {
  const decay = Math.exp(-16 * Math.max(0.001, dt));
  const t = Math.max(0, Math.min(1, interpAlpha));
  for (const entry of bodies.values()) {
    if (entry.errPos.lengthSq() > 1e-6) {
      entry.errPos.multiplyScalar(decay);
    } else {
      entry.errPos.set(0, 0, 0);
    }

    if (Math.abs(entry.errQuat.w) < 0.99999) {
      entry.errQuat.slerp(IDENTITY_QUAT, 1 - decay);
    } else {
      entry.errQuat.copy(IDENTITY_QUAT);
    }

    if (entry.rb || runtimeMode === 'authoritative') {
      tmpInterpPos.lerpVectors(entry.prevPhysPos, entry.physPos, t);
      tmpInterpQuat.slerpQuaternions(entry.prevPhysQuat, entry.physQuat, t);
      entry.meshObj.position.copy(tmpInterpPos).add(entry.errPos);
      entry.meshObj.quaternion.copy(entry.errQuat).multiply(tmpInterpQuat);
    }
  }
}

function updateCamera(dt) {
  if (!cameraSpec) return;

  if (cameraSpec.type === 'fixed') {
    camera.position.set(
      cameraSpec.pos[0],
      cameraSpec.pos[1],
      cameraSpec.pos[2]
    );
    currentLookAt.set(
      cameraSpec.lookAt[0],
      cameraSpec.lookAt[1],
      cameraSpec.lookAt[2]
    );
    camera.lookAt(currentLookAt);
    cameraInitialized = true;
    return;
  }

  if (cameraSpec.type === 'follow') {
    const target = bodies.get(cameraSpec.target);
    if (!target) return;
    const tPos = target.meshObj.position;
    const tQuat = target.meshObj.quaternion;

    const desiredPos = tmpVecA.set(
      cameraSpec.offset[0],
      cameraSpec.offset[1],
      cameraSpec.offset[2]
    );
    const desiredLook = tmpVecB.set(
      cameraSpec.lookOffset[0],
      cameraSpec.lookOffset[1],
      cameraSpec.lookOffset[2]
    );

    if (cameraSpec.local) {
      desiredPos.applyQuaternion(tQuat);
      desiredLook.applyQuaternion(tQuat);
    }
    desiredPos.add(tPos);
    desiredLook.add(tPos);

    if (!cameraInitialized) {
      camera.position.copy(desiredPos);
      currentLookAt.copy(desiredLook);
      cameraInitialized = true;
    } else {
      const alpha = 1 - Math.exp(-Math.max(0.5, cameraSpec.smooth) * dt);
      camera.position.lerp(desiredPos, alpha);
      currentLookAt.lerp(desiredLook, alpha);
    }
    camera.lookAt(currentLookAt);

    if (primaryDirLight) {
      primaryDirLight.position.copy(tPos).add(primaryDirLightOffset);
      primaryDirLight.target.position.copy(tPos);
      primaryDirLight.target.updateMatrixWorld();
    }
  }
}

window.addEventListener('keydown', (e) => {
  ensureAudio();
  if (e.repeat) return;
  if (
    ['Space', 'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Tab'].includes(
      e.code
    )
  ) {
    e.preventDefault();
  }
  const code = e.code || e.key;
  heldKeys.add(code);
  sendEvents([{ type: 'keydown', key: code }]);
});

window.addEventListener('keyup', (e) => {
  const code = e.code || e.key;
  heldKeys.delete(code);
  sendEvents([{ type: 'keyup', key: code }]);
});

window.addEventListener('blur', () => {
  if (heldKeys.size === 0) return;
  const releases = [];
  for (const code of heldKeys) {
    releases.push({ type: 'keyup', key: code });
  }
  heldKeys.clear();
  sendEvents(releases);
});

const raycaster = new THREE.Raycaster();
const ndcPointer = new THREE.Vector2();

canvas.addEventListener('pointerdown', (e) => {
  ensureAudio();
  ndcPointer.x = (e.clientX / window.innerWidth) * 2 - 1;
  ndcPointer.y = -(e.clientY / window.innerHeight) * 2 + 1;
  raycaster.setFromCamera(ndcPointer, camera);
  const hits = raycaster.intersectObjects(bodiesGroup.children, true);
  let hitPayload = null;
  for (const h of hits) {
    if (h.object && h.object.userData && h.object.userData.bodyId !== undefined) {
      hitPayload = {
        id: h.object.userData.bodyId,
        point: [h.point.x, h.point.y, h.point.z],
      };
      break;
    }
  }
  sendEvents([{ type: 'mousedown', button: e.button, hit: hitPayload }]);
});

canvas.addEventListener('pointerup', (e) => {
  sendEvents([{ type: 'mouseup', button: e.button }]);
});

canvas.addEventListener('contextmenu', (e) => {
  e.preventDefault();
});

async function main() {
  await RAPIER.init();

  const proto = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  const wsQuery = window.location.search || '';
  ws = new WebSocket(`${proto}//${window.location.host}/ws${wsQuery}`);

  ws.addEventListener('open', () => {
    wsConnected = true;
    updateStatusBadge();
    sendEvents([{ type: 'ready' }]);
  });

  ws.addEventListener('close', () => {
    wsConnected = false;
    updateStatusBadge();
  });

  ws.addEventListener('message', (msg) => {
    try {
      lastPatchBytes = typeof msg.data === 'string' ? msg.data.length : 0;
      const data = JSON.parse(msg.data);
      if (data.type === 'config') {
        if (data.mode) runtimeMode = data.mode;
      } else if (data.type === 'reset') {
        applyReset(data);
      } else if (data.type === 'delta') {
        applyDelta(data);
      } else if (data.type === 'poses') {
        applyPoses(data);
      } else if (data.type === 'takeSnapshot') {
        takeRapierSnapshot(data.tick ?? localTick);
      } else if (data.type === 'restoreSnapshot') {
        const preVisuals = captureVisualErrorOrigin();
        if (restoreRapierSnapshot(data.tick, data.snapBase64 || null)) {
          applyVisualSmoothingFrom(preVisuals);
        }
      } else if (data.type === 'replaySteps') {
        applyReplaySteps(data);
      }
    } catch (err) {
      console.error('EventHorizon failed to apply patch:', err);
    }
  });

  let lastTime = performance.now();
  let accumulator = 0;

  function frame(now) {
    requestAnimationFrame(frame);
    const dt = Math.min(0.05, (now - lastTime) / 1000);
    lastTime = now;

    if (physicsWorld) {
      let interpAlpha = 1.0;
      if (runtimeMode !== 'authoritative') {
        accumulator += dt;
        let substeps = 0;
        while (accumulator >= FIXED_DT && substeps < MAX_SUBSTEPS) {
          if (runtimeMode === 'predict') {
            takeRapierSnapshot(localTick);
            localTick += 1;
          }
          snapshotPrevPhysicsPoses();
          stepPhysics(FIXED_DT);
          readCurrentPhysicsPoses();
          accumulator -= FIXED_DT;
          substeps += 1;
        }
        interpAlpha = Math.max(0, Math.min(1, accumulator / FIXED_DT));
      } else {
        interpAlpha = advanceAuthoritativeInterpolation(dt);
      }
      syncVisuals(dt, interpAlpha);
      updateCamera(dt);
    }
    renderer.render(threeScene, camera);
  }

  requestAnimationFrame(frame);
}

main().catch((err) => {
  console.error('EventHorizon fatal startup error:', err);
  statusBadge.textContent = `Startup error: ${err.message}`;
});

// Demo: the stream player holds the Portal Gun and shows each feature in turn.
SigfShowcase("soldier")
::PG.scripted = true

::D <- { home = null, homeYaw = null, spots = [] }

// the kit's autopilot plays between scenes; during a scene it keeps out of the way
::D.pilot <- ::SigfPilotTick
::D.pauseUntil <- 0.0
::SigfPilotTick <- function(now) {
	if (now < ::D.pauseUntil) return
	::D.pilot(now)
}
// free room (0..1) along a flat ray from a standing player
::DemoRoom <- function(p, yawDeg, len) {
	local r = yawDeg / ::PG_DEG
	local from = p + Vector(0, 0, 60)
	local t = ::PG_Trace(from, from + Vector(cos(r), sin(r), 0) * len, null)
	return t.hit ? t.fraction : 1.0
}

// The staging spot for the scenes: flat ground near the start with the most open view ahead,
// room behind for the camera and open sides.
::DemoHome <- function(h) {
	local c0 = h.GetOrigin()
	local best = null
	local bestScore = -1.0
	foreach (rad in [0.0, 250.0, 450.0]) {
		for (local a = 0; a < 360; a += 45) {
			local r = a / ::PG_DEG
			local p0 = c0 + Vector(cos(r), sin(r), 0) * rad
			local fl = ::PG_Surface(p0 + Vector(0, 0, 80), Vector(0, 0, -1), h, 300.0)
			if (fl == null || fl.n.z < 0.95) continue
			local p = fl.pos + Vector(0, 0, 4)
			local hull = { start = p, end = p + Vector(0, 0, 1), hullmin = Vector(-18, -18, 0), hullmax = Vector(18, 18, 84), mask = 33636363, ignore = h }
			TraceHull(hull)
			if (hull.hit) continue
			for (local yaw = 0; yaw < 360; yaw += 45) {
				local fwd = ::DemoRoom(p, yaw, 800.0)
				local back = ::DemoRoom(p, yaw + 180.0, 340.0)
				local left = ::DemoRoom(p, yaw + 90.0, 260.0)
				local right = ::DemoRoom(p, yaw - 90.0, 260.0)
				if (back < 0.95) continue
				local score = fwd + 0.5 * (left + right) + (rad > 0.0 ? 0.3 : 0.0)
				if (score > bestScore) { bestScore = score; best = { pos = p, yaw = yaw } }
			}
		}
	}
	if (best == null) { ::D.home = c0 + Vector(0, 0, 4); return }
	::D.home = best.pos
	::D.homeYaw = best.yaw
}

// the way the player faces in the staged scenes (flat)
::DemoFwd <- function(h) {
	if (::D.homeYaw != null) {
		local r = ::D.homeYaw / ::PG_DEG
		return Vector(cos(r), sin(r), 0)
	}
	local f = h.EyeAngles().Forward()
	f.z = 0
	f.Norm()
	return f
}
::DemoScene <- function(sec, home) {
	local h = SigfHost()
	::D.pauseUntil = Time() + sec
	if (home && ::D.home != null) {
		h.Teleport(true, ::D.home, false, QAngle(0, 0, 0), true, Vector(0, 0, 0))
		if (::D.homeYaw != null) h.SnapEyeAngles(QAngle(0, ::D.homeYaw, 0))
	}
	return h
}

::DemoDir <- function(h, pos) {
	local d = pos - h.EyePosition()
	d.Norm()
	return d
}
::DemoLook <- function(h, pos) {
	local d = ::DemoDir(h, pos)
	h.SnapEyeAngles(QAngle(::PG_Pitch(d), ::PG_Yaw(d), 0))
	return d
}
::DemoFire <- function(h, spot, color) {
	local d = ::DemoLook(h, spot.pos)
	local ok = ::PG_Fire(h.entindex(), h, h.EyePosition(), d, color, 0.0)
	// a shot that cannot stick (something in the way) still puts the portal on the chosen wall
	if (!ok) ::PG_Place(h.entindex(), color, spot.pos, spot.n, ::PG_Yaw(d), 0.0)
}
// A floor spot near the player that is flat, with open sky above it for a long drop (checked as a player would
// collide: invisible clip blocks count). Returns { pos, n, rise } or null.
::DemoFloor <- function(h, minRise) {
	local f = ::DemoFwd(h)
	local side = Vector(-f.y, f.x, 0)
	local best = null
	foreach (dist in [230.0, 320.0, 160.0, 400.0]) {
		foreach (sd in [0.0, 150.0, -150.0, 300.0, -300.0]) {
			local at = h.GetOrigin() + f * dist + side * sd
			local fl = ::PG_Surface(at + Vector(0, 0, 80), Vector(0, 0, -1), h, 400.0)
			if (fl == null || fl.n.z < 0.95) continue
			local q = ::PG_Fit(fl.pos, fl.n)
			if (q == null) continue
			local tr = { start = q + Vector(0, 0, 10), end = q + Vector(0, 0, 730), hullmin = Vector(-18, -18, 0), hullmax = Vector(18, 18, 84), mask = 33636363, ignore = h }
			TraceHull(tr)
			local rise = 10.0 + 720.0 * (tr.hit ? tr.fraction : 1.0) - 12.0
			if (best == null || rise > best.rise) best = { pos = q, n = fl.n, rise = rise }
			if (rise >= minRise) return best
		}
	}
	return best
}
::D.foe <- null
// an enemy standing still in front of the player (a new one when the last is dead)
::DemoFoe <- function(h, dist) {
	local e = ::D.foe
	if (e == null || !e.IsValid() || !e.IsAlive()) {
		e = SigfBringEnemy(dist)
		::D.foe = e
	}
	if (e == null) return null
	local f = ::DemoFwd(h)
	e.Teleport(true, h.GetOrigin() + f * dist + Vector(0, 0, 8), false, QAngle(0, 0, 0), true, Vector(0, 0, 0))
	::DemoFreeze(e, 6.0)
	return e
}
::DemoFlat <- function(h, pos) {
	local d = pos - h.GetOrigin()
	h.SnapEyeAngles(QAngle(0, ::PG_Yaw(d), 0))
}
// yaw of a spot seen from the player, relative to where the player looks (-180..180)
::DemoRel <- function(h, spot) {
	local d = spot.pos - h.GetOrigin()
	local rel = ::PG_Yaw(d) - ::PG_Yaw(::DemoFwd(h))
	while (rel > 180) rel -= 360
	while (rel < -180) rel += 360
	return rel
}
::DemoFreeze <- function(e, sec) {
	if (e != null && e.IsValid()) e.AddCustomAttribute("move speed bonus", 0.02, sec)
}
// pick two spots: one in front, one more to the side, far enough apart
::DemoPair <- function(h) {
	local spots = ::PG_Spots(h.GetOrigin(), 150.0, 1100.0, h)
	local a = null
	local b = null
	// walkable wall at about doorway height, close by
	local score = function(s) { return s.dist + 4.0 * fabs(s.pos.z - h.GetOrigin().z - 62.0) }
	foreach (s in spots) {
		if (s.clear && fabs(::DemoRel(h, s)) < 100 && (a == null || score(s) < score(a))) a = s
	}
	if (a == null) foreach (s in spots) if (s.clear && (a == null || score(s) < score(a))) a = s
	if (a == null) foreach (s in spots) if (a == null || s.dist < a.dist) a = s
	// the exit faces open ground, so the player comes out into the action
	foreach (s in spots) {
		if (a == null || (s.pos - a.pos).Length() < 400.0 || s.open < 450.0) continue
		if (b == null || s.dist < b.dist) b = s
	}
	if (b == null) foreach (s in spots) if (a != null && (s.pos - a.pos).Length() >= 300.0 && (b == null || s.open > b.open)) b = s
	// an entrance at doorway height with a clear walk to it: stand 230 units in front of a wall, on the floor there
	local door = null
	foreach (s in spots) {
		if (door != null) break
		local sp = s.pos + s.n * 230.0
		local fl = ::PG_Surface(sp + Vector(0, 0, 60), Vector(0, 0, -1), h, 220.0)
		if (fl == null || fl.n.z < 0.95) continue
		local q = ::PG_Fit(Vector(s.pos.x, s.pos.y, fl.pos.z + 62.0), s.n)
		if (q == null || fabs(q.z - fl.pos.z - 62.0) > 12.0) continue
		local walk = { start = fl.pos + Vector(0, 0, 20), end = q + s.n * 28.0 - Vector(0, 0, q.z - fl.pos.z - 20.0), hullmin = Vector(-16, -16, 0), hullmax = Vector(16, 16, 60), mask = 33636363, ignore = h }
		TraceHull(walk)
		if (walk.hit && walk.fraction < 0.97) continue
		door = { pos = q, n = s.n, dist = s.dist, yaw = s.yaw, clear = true, open = s.open, stand = fl.pos + Vector(0, 0, 4) }
	}
	if (door != null) {
		a = door
		b = null
		foreach (s in spots) {
			if ((s.pos - a.pos).Length() < 400.0 || s.open < 450.0) continue
			if (b == null || s.dist < b.dist) b = s
		}
		if (b == null) foreach (s in spots) if ((s.pos - a.pos).Length() >= 300.0 && (b == null || s.open > b.open)) b = s
	}
	return [a, b]
}
::DemoCubes <- function(h, n) {
	local f = ::DemoFwd(h)
	local side = Vector(-f.y, f.x, 0)
	for (local i = 0; i < n; i++) {
		local p = h.GetOrigin() + f * (150.0 + 40.0 * i) + side * ((i - (n - 1) / 2.0) * 70.0) + Vector(0, 0, 60)
		::PG_SpawnCube(p)
		DispatchParticleEffect("teleported_blue", p, Vector(0, 0, 0))
	}
}
::DemoGrab <- function(h, withFoe = true) {
	local best = null
	local bestD = 9999.0
	foreach (cr in ::PG.cubes) {
		if (!cr.ent.IsValid()) continue
		local d = (cr.ent.GetCenter() - h.GetOrigin()).Length()
		if (d < bestD) { best = cr; bestD = d }
	}
	if (best == null) return
	local d = ::DemoLook(h, best.ent.GetCenter())
	::PG_Grab(h, d, null)
	if (withFoe) ::DemoFoe(h, 430.0)
}
// bring a standing enemy in front, look at it, throw the carried cube
::DemoThrowAtFoe <- function(h) {
	local e = ::DemoFoe(h, 430.0)
	if (e == null) return
	::DemoLook(h, e.GetCenter())
	SigfIn(0.25, function() {
		if (e.IsValid()) ::PG_Throw(h, ::DemoDir(h, e.GetCenter()))
	})
}

// 0.2 s: remember where the show starts
// The scenes run from the start of the recording, then repeat every 60 s; if the recording flag never comes,
// they start 6 s after the player is in the game.
::D.CYC <- 60.0
::D.steps <- []
::D.ready <- -1.0
::DemoStep <- function(sec, fn) {
	local st = { sec = sec, fn = fn, last = -999.0 }
	::D.steps.append(st)
	SigfDemo(sec, function() { st.last = Time(); fn() })
}
SigfEvery(0.25, function() {
	if (!::SigfShowReady) return
	local now = Time()
	if (::D.ready < 0) ::D.ready = now
	local t0 = ::SigfRecAt >= 0 ? ::SigfRecAt : (now - ::D.ready > 6.0 ? ::D.ready + 6.0 : -1.0)
	if (t0 < 0 || now < t0) return
	foreach (st in ::D.steps) {
		local n = floor((now - t0 - st.sec) / ::D.CYC)
		if (n < 0) continue
		local due = t0 + st.sec + n * ::D.CYC
		if (st.last < due - 0.05) {
			st.last = now
			if (st.sec < 0.5 && n > 0) continue
			try { st.fn() } catch (e) { printl("SIGF_ERROR demo scene " + st.sec + ": " + e) }
		}
	}
})
::DemoStep(0.2, function() { ::DemoHome(SigfHost()) })

// 1 s: portals on two walls, then walk through one
::DemoStep(1, function() {
	local h = ::DemoScene(8.0, false)
	SigfCaption("PORTAL GUN: shoot a blue and an orange portal on any wall", 4)
	local pair = ::DemoPair(h)
	local a = pair[0]
	local b = pair[1]
	if (a == null) return
	if ("stand" in a) {
		h.Teleport(true, a.stand, false, QAngle(0, 0, 0), true, Vector(0, 0, 0))
	}
	::DemoFire(h, a, 0)
	// two Companion Cubes sit in front of the player from the first seconds
	if ("stand" in a) {
		local side = Vector(-a.n.y, a.n.x, 0)
		foreach (o in [-75.0, 75.0]) {
			local p = a.stand + a.n * -130.0 + side * o + Vector(0, 0, 60)
			::PG_SpawnCube(p)
			DispatchParticleEffect("teleported_blue", p, Vector(0, 0, 0))
		}
	}
	if (b == null) return
	SigfIn(1.6, function() { ::DemoFire(h, b, 1) })
	SigfIn(3.4, function() {
		::DemoFlat(h, a.pos)
		::SigfPilotHold("forward", 2.6)
	})
	SigfIn(4.4, function() { SigfCaption("Walk into one, step out of the other", 3) })
})

// 11 s: fall in a floor portal, fly out of a wall portal at the same speed
::DemoStep(11, function() {
	local h = ::DemoScene(7.0, true)
	SigfCaption("MOMENTUM IS KEPT: fall in fast, fly out faster", 5)
	local fl = ::DemoFloor(h, 250.0)
	if (fl == null) return
	local spots = ::PG_Spots(h.GetOrigin(), 350.0, 1200.0, h)
	local w = null
	foreach (s in spots) if (s.open >= 500.0 && (w == null || s.dist > w.dist)) w = s
	if (w == null) foreach (s in spots) if (w == null || s.open > w.open) w = s
	if (w == null) return
	local key = h.entindex()
	::PG_Place(key, 0, fl.pos, Vector(0, 0, 1), 0.0, 0.0)
	::PG_Place(key, 1, w.pos, w.n, 0.0, 0.0)
	SigfIn(1.2, function() {
		// as high as the sky above allows, then dropped hard
		local top = fl.pos + Vector(0, 0, fl.rise)
		h.Teleport(true, top, false, QAngle(0, 0, 0), true, Vector(0, 0, -1100))
		local d = w.pos - top
		h.SnapEyeAngles(QAngle(0, ::PG_Yaw(d), 0))
	})
})

// 20 s: grab companion cubes and throw them at an enemy
::DemoStep(20, function() {
	local h = ::DemoScene(10.0, true)
	SigfCaption("COMPANION CUBES: grab one, carry it, throw it", 5)
	::DemoCubes(h, 3)
	SigfIn(1.2, function() { ::DemoGrab(h) })
	SigfIn(2.6, function() { ::DemoThrowAtFoe(h) })
	SigfIn(4.4, function() { ::DemoGrab(h) })
	SigfIn(5.8, function() { ::DemoThrowAtFoe(h) })
	SigfIn(7.4, function() { ::DemoGrab(h) })
	SigfIn(8.8, function() { ::DemoThrowAtFoe(h) })
})

// 32 s: throw a cube into the blue portal, it comes out of the orange one and hits the enemy
::DemoStep(32, function() {
	local h = ::DemoScene(10.0, true)
	SigfCaption("THINK WITH PORTALS: the cube goes in here, comes out there", 5)
	local spots = ::PG_Spots(h.GetOrigin(), 150.0, 1300.0, h)
	local a = null
	local b = null
	foreach (s in spots) if (s.clear && fabs(::DemoRel(h, s)) < 90 && (a == null || s.dist < a.dist)) a = s
	if (a == null) foreach (s in spots) if (a == null || s.dist < a.dist) a = s
	if (a == null) return
	foreach (s in spots) {
		if ((s.pos - a.pos).Length() > 300.0 && (b == null || s.dist < b.dist)) b = s
	}
	if (b == null) return
	local key = h.entindex()
	::PG_Place(key, 0, a.pos, a.n, 0.0, 0.0)
	::PG_Place(key, 1, b.pos, b.n, 0.0, 0.0)
	::DemoCubes(h, 1)
	// the enemy waits in front of the orange portal; put back there just before each throw
	local standFoe = function() {
		local e = ::D.foe
		if (e == null || !e.IsValid() || !e.IsAlive()) {
			e = SigfBringEnemy(300)
			::D.foe = e
		}
		if (e == null) return
		e.Teleport(true, b.pos + b.n * 130.0 - Vector(0, 0, 55), false, QAngle(0, 0, 0), true, Vector(0, 0, 0))
		::DemoFreeze(e, 4.0)
	}
	SigfIn(1.0, function() { standFoe(); ::DemoLook(h, a.pos) })
	SigfIn(3.6, function() { standFoe() })
	SigfIn(6.8, function() { standFoe() })
	SigfIn(2.4, function() { ::DemoGrab(h, false) })
	SigfIn(3.8, function() {
		::DemoLook(h, a.pos)
		SigfIn(0.2, function() { ::PG_Throw(h, ::DemoDir(h, a.pos)) })
	})
	SigfIn(5.8, function() { ::DemoGrab(h, false) })
	SigfIn(7.0, function() {
		::DemoLook(h, a.pos)
		SigfIn(0.2, function() { ::PG_Throw(h, ::DemoDir(h, a.pos)) })
	})
})

// 44 s: cubes pour through a floor portal and shoot out of a wall portal
::DemoStep(44, function() {
	local h = ::DemoScene(14.0, true)
	SigfCaption("ENDLESS CUBES: a portal in the floor, a portal in the wall", 5)
	local f = ::DemoFwd(h)
	local fl = ::DemoFloor(h, 300.0)
	local spots = ::PG_Spots(h.GetOrigin(), 450.0, 1100.0, h)
	local w = null
	foreach (s in spots) if (s.open >= 300.0 && fabs(::DemoRel(h, s)) < 110 && (w == null || s.dist < w.dist)) w = s
	if (w == null) foreach (s in spots) if (w == null || s.open > w.open) w = s
	if (fl == null || w == null) return
	local key = h.entindex()
	::PG_Place(key, 0, fl.pos, Vector(0, 0, 1), 0.0, 0.0)
	::PG_Place(key, 1, w.pos, w.n, 0.0, 0.0)
	for (local i = 0; i < 5; i++) {
		SigfIn(1.0 + i * 0.9, function() {
			local p = fl.pos + Vector(RandomFloat(-14, 14), RandomFloat(-14, 14), 360)
			local cr = ::PG_SpawnCube(p)
			cr.thrower = h
			cr.thrownUntil = Time() + 4.0
			cr.ent.SetPhysVelocity(Vector(0, 0, -300))
			EntFireByHandle(cr.ent, "Kill", "", 11.0, null, null)
		})
	}
	SigfIn(7.0, function() { SigfCaption("PORTAL GUN AND COMPANION CUBES", 6) })
})

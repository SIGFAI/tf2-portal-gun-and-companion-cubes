// Portal Gun and Companion Cubes.
// Portal Gun = the melee slot (key 3): click = blue portal, right click = orange portal, R = grab / drop a cube, click while carrying = throw.
// Everything that touches a portal (players, bots, cubes, rockets, grenades) comes out of the other one with its speed kept.

::PG <- {
	portals = [], cubes = [], lv = {}, cool = {}, carry = {}, hit = {},
	btn = 0, scripted = false, gunGlow = null, nextBot = 0.0, nextCube = 0.0, nextSlow = 0.0, nextEquip = 0.0, nextGun = 0.0,
}
::PG_MASK <- 33570827
::PG_CUBE <- "models/props_island/mannco_case_small.mdl"
::PG_DEG <- 57.29578

::PG_Yaw <- function(v) { return atan2(v.y, v.x) * ::PG_DEG }
::PG_Pitch <- function(v) { return atan2(-v.z, sqrt(v.x * v.x + v.y * v.y)) * ::PG_DEG }

::PG_Trace <- function(a, b, ign) {
	local t = { start = a, end = b, mask = ::PG_MASK }
	if (ign != null) t.ignore <- ign
	TraceLineEx(t)
	return t
}

// First solid surface along a ray. Players do not block it; cubes and sky do.
::PG_Surface <- function(from, dir, ign, range) {
	local start = from
	local to = from + dir * range
	for (local i = 0; i < 5; i++) {
		local t = ::PG_Trace(start, to, ign)
		if (!t.hit) return null
		local e = t.enthit
		if (e != null && e.IsValid()) {
			if (e.IsPlayer()) { start = t.pos + dir * 4.0; continue }
			if (e.GetClassname().find("prop_physics") == 0) return null
		}
		if ((t.surface_flags & 6) != 0) return null
		return { pos = t.pos, n = t.plane_normal }
	}
	return null
}

// Is the plane flat and empty enough around q for a portal?
::PG_Flat <- function(q, n, t1, t2, ha, hb) {
	local spots = [[0.0, 0.0], [ha, 0.0], [-ha, 0.0], [0.0, hb], [0.0, -hb], [ha * 0.7, hb * 0.7], [-ha * 0.7, hb * 0.7], [ha * 0.7, -hb * 0.7], [-ha * 0.7, -hb * 0.7]]
	foreach (o in spots) {
		local s = q + t1 * o[0] + t2 * o[1]
		local tr = ::PG_Trace(s + n * 10.0, s - n * 10.0, null)
		if (!tr.hit || tr.fraction < 0.28 || tr.fraction > 0.72) return false
		if (tr.plane_normal.Dot(n) < 0.9) return false
	}
	local front = ::PG_Trace(q + n * 4.0, q + n * 34.0, null)
	if (front.hit && !(front.enthit != null && front.enthit.IsValid() && front.enthit.IsPlayer())) return false
	return true
}

// Find a spot on this surface where a portal fits (nudges it, lifts wall portals off the floor). null = does not fit.
::PG_Fit <- function(p, n) {
	local wall = fabs(n.z) < 0.5
	local t1 = null
	local t2 = null
	local ha = 0.0
	local hb = 0.0
	local bp = Vector(p.x, p.y, p.z)
	if (wall) {
		t1 = n.Cross(Vector(0, 0, 1))
		t1.Norm()
		t2 = Vector(0, 0, 1)
		ha = 24.0
		hb = 52.0
		local f = ::PG_Trace(p + n * 6.0, p + n * 6.0 + Vector(0, 0, -300), null)
		local above = f.hit ? 300.0 * f.fraction : 999.0
		if (above < 62.0) bp.z += 62.0 - above
	} else {
		t1 = Vector(1, 0, 0)
		t2 = Vector(0, 1, 0)
		ha = 26.0
		hb = 26.0
	}
	local nudges = [[0.0, 0.0], [0.0, 40.0], [0.0, -40.0], [30.0, 0.0], [-30.0, 0.0], [30.0, 40.0], [-30.0, 40.0], [30.0, -40.0], [-30.0, -40.0]]
	foreach (o in nudges) {
		local q = bp + t1 * o[0] + t2 * o[1]
		if (::PG_Flat(q, n, t1, t2, ha, hb)) return q
	}
	return null
}

::PG_Find <- function(key, color) {
	foreach (o in ::PG.portals) if (o.key == key && o.color == color && o.ent.IsValid()) return o
	return null
}

// Blue/orange glowing dots from the gun to the target.
::PG_Shot <- function(a, b, color) {
	local mat = color == 0 ? "sigf/orb_blue" : "sigf/orb_orange"
	local d = b - a
	local len = d.Length()
	local n = len > 80.0 ? 9 : 3
	for (local i = 0; i <= n; i++) {
		local at = a + d * (i / (n + 1.0)) + Vector(0, 0, -6)
		::SigfSprite(mat, at, 0.07 + 0.05 * (i / (n + 1.0)), 0.3)
	}
	::SigfSprite(mat, b, 0.3, 0.45)
	::SigfSound(color == 0 ? "sigf/pg_blue.wav" : "sigf/pg_orange.wav", a, 82)
}

::PG_Place <- function(key, color, p, n, yawDeg, life) {
	::PG.portals = ::PG.portals.filter(function(i, o) {
		if (!o.ent.IsValid()) return false
		if ((o.key == key && o.color == color) || ((o.pos - p).Length() < 50.0 && o.n.Dot(n) > 0.9)) { o.ent.Kill(); return false }
		return true
	})
	// bots share a few pairs: the oldest bot pair goes first
	local bots = []
	foreach (o in ::PG.portals) if (o.die > 0 && o.key != key && bots.find(o.key) == null) bots.append(o.key)
	if (bots.len() > 2) {
		local gone = bots[0]
		::PG.portals = ::PG.portals.filter(function(i, o) {
			if (o.key == gone) { o.ent.Kill(); return false }
			return true
		})
	}
	local wall = fabs(n.z) < 0.5
	local t1 = null
	local t2 = null
	if (wall) {
		t1 = n.Cross(Vector(0, 0, 1))
		t1.Norm()
		t2 = Vector(0, 0, 1)
	} else {
		t1 = Vector(1, 0, 0)
		t2 = Vector(0, 1, 0)
	}
	local pitch = ::PG_Pitch(n)
	local yaw = wall ? ::PG_Yaw(n) : yawDeg
	local ent = SpawnEntityFromTable("env_sprite", {
		model = color == 0 ? "sigf/portal_blue.vmt" : "sigf/portal_orange.vmt", origin = p + n * 1.2,
		angles = Vector(pitch, yaw, 0), scale = 0.36, rendermode = 5, spawnflags = 1,
	})
	ent.SetAbsAngles(QAngle(pitch, yaw, 0))
	// a second copy on top doubles the glow so the ring reads clearly in daylight
	local twin = SpawnEntityFromTable("env_sprite", {
		model = color == 0 ? "sigf/portal_blue.vmt" : "sigf/portal_orange.vmt", origin = p + n * 1.6,
		angles = Vector(pitch, yaw, 0), scale = 0.36, rendermode = 5, spawnflags = 1,
	})
	twin.SetAbsAngles(QAngle(pitch, yaw, 0))
	twin.AcceptInput("SetParent", "!activator", ent, ent)
	::PG.portals.append({ key = key, color = color, pos = p, n = n, t1 = t1, t2 = t2, wall = wall, ent = ent, die = life > 0 ? Time() + life : -1.0 })
	DispatchParticleEffect(color == 0 ? "teleported_blue" : "teleported_red", p + n * 10.0, Vector(0, 0, 0))
	::SigfSound("sigf/portal_open.wav", p, 80)
}

// Fire one portal from a point along a direction. Returns true when it stuck.
::PG_Fire <- function(key, shooter, from, dir, color, life) {
	local s = ::PG_Surface(from, dir, shooter, 5000.0)
	local fizzle = function(at) {
		::SigfSound("sigf/pg_fizzle.wav", at, 78)
		DispatchParticleEffect("impact_metal", at, Vector(0, 0, 0))
	}
	if (s == null) { ::PG_Shot(from, from + dir * 700.0, color); fizzle(from + dir * 700.0); return false }
	::PG_Shot(from, s.pos, color)
	local q = ::PG_Fit(s.pos, s.n)
	if (q == null) { fizzle(s.pos); return false }
	local mate = ::PG_Find(key, 1 - color)
	if (mate != null && (mate.pos - q).Length() < 70.0 && mate.n.Dot(s.n) > 0.9) { fizzle(s.pos); return false }
	::PG_Place(key, color, q, s.n, ::PG_Yaw(dir), life)
	return true
}

// Portals on walls around a spot (used by bots and the demo). Each = { pos, n, dist, yaw }.
::PG_Spots <- function(center, minD, maxD, ign) {
	local out = []
	for (local a = 0; a < 360; a += 20) {
		local r = a / ::PG_DEG
		local from = center + Vector(0, 0, 56)
		local s = ::PG_Surface(from, Vector(cos(r), sin(r), 0), ign, maxD)
		if (s == null || fabs(s.n.z) > 0.15) continue
		local d = (s.pos - from).Length()
		if (d < minD) continue
		local q = ::PG_Fit(s.pos, s.n)
		if (q == null) continue
		// walkable: a low ray reaches the same wall at about the same distance
		local low = ::PG_Trace(center + Vector(0, 0, 18), center + Vector(0, 0, 18) + Vector(cos(r), sin(r), 0) * maxD, ign)
		local lowD = low.hit ? maxD * low.fraction : maxD
		local fr = ::PG_Trace(q + s.n * 40.0, q + s.n * 740.0, ign)
		out.append({ pos = q, n = s.n, dist = d, yaw = a, clear = fabs(lowD - d) < 24.0, open = fr.hit ? 700.0 * fr.fraction : 700.0 })
	}
	return out
}

// ---- crossing a portal ----

::PG_RotFor <- function(an, bn) {
	local a = an * -1.0
	local c = a.Dot(bn)
	local ax = a.Cross(bn)
	local sn = ax.Length()
	if (sn < 0.001) {
		if (c > 0) return { axis = Vector(0, 0, 1), c = 1.0, s = 0.0 }
		return { axis = fabs(an.z) > 0.9 ? Vector(1, 0, 0) : Vector(0, 0, 1), c = -1.0, s = 0.0 }
	}
	return { axis = ax * (1.0 / sn), c = c, s = sn }
}
::PG_Rot <- function(r, v) { return v * r.c + r.axis.Cross(v) * r.s + r.axis * (r.axis.Dot(v) * (1.0 - r.c)) }

::PG_IsCarried <- function(e) {
	foreach (k, cr in ::PG.carry) if (cr.cube.ent == e) return true
	return false
}

::PG_Pass <- function(e, kind, A, B, prev) {
	local now = Time()
	local vel = prev
	local minIn = kind == 0 ? 280.0 : (kind == 1 ? 170.0 : 0.0)
	local vn = vel.Dot(A.n)
	if (vn > -minIn) vel = vel + A.n * (-minIn - vn)
	local r = ::PG_RotFor(A.n, B.n)
	local nv = ::PG_Rot(r, vel)
	local out = nv.Dot(B.n)
	local minOut = kind == 2 ? 100.0 : 220.0
	if (out < minOut) nv = nv + B.n * (minOut - out)

	local c = e.GetCenter()
	local rel = c - A.pos
	local rt = rel - A.n * rel.Dot(A.n)
	rt = ::PG_Rot(r, rt)
	rt = rt - B.n * rt.Dot(B.n)
	local rl = rt.Length()
	if (rl > 22.0) rt = rt * (22.0 / rl)
	local off = kind == 0 ? 42.0 : (kind == 1 ? 36.0 : 30.0)
	local nc = B.pos + B.n * off + rt
	local delta = e.GetOrigin() - c
	e.Teleport(true, nc + delta, false, QAngle(0, 0, 0), true, nv)
	if (kind == 1) e.SetPhysVelocity(nv)
	if (kind == 2) e.SetForwardVector(nv * (1.0 / nv.Length()))
	if (kind == 0) {
		local f = ::PG_Rot(r, e.EyeAngles().Forward())
		local pitch = ::PG_Pitch(f)
		// coming out of a wall the view stays level enough to see where you are going
		if (fabs(B.n.z) < 0.5) pitch = pitch > 20.0 ? 20.0 : (pitch < -20.0 ? -20.0 : pitch)
		e.SnapEyeAngles(QAngle(pitch, ::PG_Yaw(f), 0))
	}
	::PG.cool[e.entindex()] <- now + 0.45
	::PG.lv[e.entindex()] <- nv
	DispatchParticleEffect(A.color == 0 ? "teleported_blue" : "teleported_red", A.pos + A.n * 12.0, Vector(0, 0, 0))
	DispatchParticleEffect(B.color == 0 ? "teleported_blue" : "teleported_red", B.pos + B.n * 12.0, Vector(0, 0, 0))
	::SigfSound("sigf/portal_pass.wav", A.pos, 80)
	::SigfSound("sigf/portal_pass.wav", B.pos, 80)
}

::PG_Check <- function(e, kind, A, B, now) {
	local idx = e.entindex()
	local cur = kind == 1 ? e.GetPhysVelocity() : e.GetAbsVelocity()
	local prev = (idx in ::PG.lv) ? ::PG.lv[idx] : cur
	::PG.lv[idx] <- cur
	if ((idx in ::PG.cool) && ::PG.cool[idx] > now) return
	local rel = e.GetCenter() - A.pos
	local d = rel.Dot(A.n)
	local l1 = rel.Dot(A.t1)
	local l2 = rel.Dot(A.t2)
	local inside = A.wall ? ((l1 * l1) / 1156.0 + (l2 * l2) / 4096.0 <= 1.0) : (sqrt(l1 * l1 + l2 * l2) <= 36.0)
	if (!inside) return
	local vn = prev.Dot(A.n)
	if (kind == 0) {
		if (d > (A.wall ? 34.0 : 58.0) || d < -24.0) return
		if (A.wall) {
			local pushing = (NetProps.GetPropInt(e, "m_nButtons") & 8) != 0 && e.EyeAngles().Forward().Dot(A.n) < -0.3
			if (!(vn < -60.0 || pushing)) return
		} else if (vn > 40.0) return
	} else if (kind == 1) {
		if (d > 42.0 || d < -26.0) return
		if (!A.wall && vn > 30.0) return
		if (::PG_IsCarried(e)) return
	} else {
		if (d > 70.0 || d < -30.0 || vn > -80.0) return
	}
	::PG_Pass(e, kind, A, B, prev)
}

::PG_Kind <- function(e) {
	if (e.IsPlayer()) return e.IsAlive() ? 0 : -1
	local c = e.GetClassname()
	if (c.find("tf_projectile") == 0) return 2
	if (c.find("prop_physics") == 0) return 1
	return -1
}

::PG_Transit <- function(now) {
	::PG.portals = ::PG.portals.filter(@(i, o) o.ent.IsValid() && (o.die < 0 || o.die > now))
	foreach (A in ::PG.portals) {
		local B = ::PG_Find(A.key, 1 - A.color)
		if (B == null) continue
		for (local e = Entities.FindInSphere(null, A.pos, 110.0); e != null; e = Entities.FindInSphere(e, A.pos, 110.0)) {
			local kind = ::PG_Kind(e)
			if (kind >= 0) ::PG_Check(e, kind, A, B, now)
		}
	}
}

// ---- Companion Cubes ----

::PG_SpawnCube <- function(pos) {
	PrecacheModel(::PG_CUBE)
	PrecacheModel("sigf/cube_face.vmt")
	local body = SpawnEntityFromTable("prop_physics_override", { model = ::PG_CUBE, origin = pos, rendermode = 10, disableshadows = 1 })
	local faces = []
	local dirs = [Vector(1, 0, 0), Vector(-1, 0, 0), Vector(0, 1, 0), Vector(0, -1, 0), Vector(0, 0, 1), Vector(0, 0, -1)]
	local shade = ["235 235 235", "205 205 205", "220 220 220", "190 190 190", "255 255 255", "150 150 150"]
	for (local i = 0; i < 6; i++) {
		local n = dirs[i]
		local f = SpawnEntityFromTable("env_sprite", {
			model = "sigf/cube_face.vmt", origin = pos + n * 24.3, angles = Vector(::PG_Pitch(n), ::PG_Yaw(n), 0),
			scale = 0.19, rendermode = 0, rendercolor = shade[i], spawnflags = 1,
		})
		f.SetAbsAngles(QAngle(::PG_Pitch(n), ::PG_Yaw(n), 0))
		f.AcceptInput("SetParent", "!activator", body, body)
		faces.append(f)
	}
	local cr = { ent = body, faces = faces, thrower = null, thrownUntil = 0.0 }
	::PG.cubes.append(cr)
	return cr
}

::PG_CubeOf <- function(ent) {
	foreach (cr in ::PG.cubes) if (cr.ent == ent) return cr
	return null
}

// nearest cube nobody holds, within range of a holder's eyes and in front of them
::PG_NearCube <- function(holder, range, aim) {
	local eye = holder.EyePosition()
	local best = null
	local bestD = range
	foreach (cr in ::PG.cubes) {
		if (!cr.ent.IsValid() || ::PG_IsCarried(cr.ent)) continue
		local d = cr.ent.GetCenter() - eye
		local dist = d.Length()
		if (dist < bestD && dist > 1.0 && (aim == null || d.Dot(aim) / dist > 0.35)) { best = cr; bestD = dist }
	}
	return best
}

// selectDir: where to look for a cube (null = where the holder looks). holdDir: fixed carry direction (null = follow the view).
::PG_Grab <- function(holder, selectDir, holdDir) {
	local k = holder.entindex()
	if (k in ::PG.carry) { ::PG_Drop(holder); return true }
	local aim = selectDir != null ? selectDir : holder.EyeAngles().Forward()
	local cr = ::PG_NearCube(holder, 260.0, aim)
	if (cr == null) return false
	::PG.carry[k] <- { holder = holder, cube = cr, aim = holdDir, t0 = Time() }
	::SigfSound("sigf/cube_grab.wav", cr.ent.GetCenter(), 80)
	return true
}

::PG_Drop <- function(holder) {
	local k = holder.entindex()
	if (k in ::PG.carry) delete ::PG.carry[k]
}

::PG_Throw <- function(holder, dir) {
	local k = holder.entindex()
	if (!(k in ::PG.carry)) return false
	local cr = ::PG.carry[k].cube
	delete ::PG.carry[k]
	if (!cr.ent.IsValid()) return false
	local v = dir * 1500.0 + Vector(0, 0, 90) + holder.GetAbsVelocity() * 0.4
	cr.ent.SetPhysVelocity(v)
	cr.thrower = holder
	cr.thrownUntil = Time() + 3.0
	::SigfSound("sigf/pg_orange.wav", cr.ent.GetCenter(), 74)
	return true
}

::PG_CarryTick <- function() {
	local gone = []
	foreach (k, cr in ::PG.carry) {
		local h = cr.holder
		local c = cr.cube.ent
		if (!h.IsValid() || !h.IsAlive() || !c.IsValid()) { gone.append(k); continue }
		local fwd = cr.aim != null ? cr.aim : h.EyeAngles().Forward()
		local target = h.EyePosition() + fwd * 84.0 + Vector(0, 0, -16)
		local diff = target - c.GetCenter()
		if (diff.Length() > 280.0) {
			c.Teleport(true, target, false, QAngle(0, 0, 0), true, Vector(0, 0, 0))
		} else {
			local v = diff * 14.0
			local sp = v.Length()
			if (sp > 1300.0) v = v * (1300.0 / sp)
			c.SetPhysVelocity(v)
			c.SetPhysAngularVelocity(Vector(0, 0, 0))
		}
	}
	foreach (k in gone) delete ::PG.carry[k]
}

// A flying cube hurts the enemies it hits: shove, blood, thud.
::PG_HitTick <- function(now) {
	foreach (cr in ::PG.cubes) {
		if (!cr.ent.IsValid() || cr.thrownUntil < now || ::PG_IsCarried(cr.ent)) continue
		local vel = cr.ent.GetPhysVelocity()
		local speed = vel.Length()
		if (speed < 380.0) continue
		local c = cr.ent.GetCenter()
		// a thrown cube bends a little toward an enemy it is already flying at
		local th0 = cr.thrower
		if (speed > 500.0 && th0 != null && th0.IsValid()) {
			local best = null
			local bestDot = 0.93
			foreach (p in SigfPlayers()) {
				if (p.GetTeam() == th0.GetTeam()) continue
				local to = p.GetCenter() - c
				local dist = to.Length()
				if (dist < 60.0 || dist > 650.0) continue
				local dot = to.Dot(vel) / (dist * speed)
				if (dot > bestDot) { best = p; bestDot = dot }
			}
			if (best != null) {
				local want = best.GetCenter() - c
				want.Norm()
				local nd = vel * (0.75 / speed) + want * 0.25
				nd.Norm()
				vel = nd * speed
				cr.ent.SetPhysVelocity(vel)
			}
		}
		for (local e = Entities.FindInSphere(null, c, 58.0); e != null; e = Entities.FindInSphere(e, c, 58.0)) {
			if (!e.IsPlayer() || !e.IsAlive()) continue
			local th = cr.thrower
			if (th != null && th.IsValid() && (e == th || e.GetTeam() == th.GetTeam())) continue
			local id = e.entindex()
			if ((id in ::PG.hit) && ::PG.hit[id] > now) continue
			::PG.hit[id] <- now + 0.6
			local dmg = speed * 0.065
			if (dmg > 85.0) dmg = 85.0
			if (dmg < 30.0) dmg = 30.0
			local dir = vel * (1.0 / speed)
			e.TakeDamage(dmg, 128, (th != null && th.IsValid()) ? th : cr.ent)
			e.ApplyAbsVelocityImpulse(dir * 380.0 + Vector(0, 0, 200))
			DispatchParticleEffect("blood_impact_red_01", e.GetCenter(), Vector(0, 0, 0))
			DispatchParticleEffect("impact_metal", c, Vector(0, 0, 0))
			::SigfSound("sigf/cube_hit.wav", c, 88)
			cr.ent.SetPhysVelocity(vel * -0.3 + Vector(0, 0, 160))
			cr.thrownUntil = now + 0.4
			break
		}
	}
}

::PG_CubeUpkeep <- function(now) {
	::PG.cubes = ::PG.cubes.filter(function(i, cr) {
		if (!cr.ent.IsValid()) return false
		if (cr.ent.GetOrigin().z < -2500.0) { cr.ent.Kill(); return false }
		return true
	})
	if (::PG.cubes.len() < 5 && now > ::PG.nextCube) {
		::PG.nextCube = now + 4.0
		// cubes land near the fighters, where the camera is
		local ps = ::SigfPlayers()
		local p = ::SigfGround() + Vector(0, 0, 160)
		if (ps.len()) {
			local q = ps[RandomInt(0, ps.len() - 1)]
			local r = RandomFloat(0.0, 6.28)
			p = q.GetOrigin() + Vector(cos(r), sin(r), 0) * 160.0 + Vector(0, 0, 110)
		}
		::PG_SpawnCube(p)
		DispatchParticleEffect("teleported_blue", p, Vector(0, 0, 0))
		::SigfSound("sigf/portal_open.wav", p, 78)
	}
}

// ---- bots use the gun and the cubes too ----

::PG_BotShot <- function(key, shooter, color) {
	for (local i = 0; i < 6; i++) {
		local r = RandomFloat(0.0, 6.28)
		local dir = Vector(cos(r), sin(r), RandomFloat(-0.15, 0.2))
		dir.Norm()
		local from = shooter.EyePosition()
		local s = ::PG_Surface(from, dir, shooter, 900.0)
		if (s != null && (s.pos - from).Length() > 120.0 && ::PG_Fire(key, shooter, from, dir, color, 40.0)) return true
	}
	return false
}

::PG_NearestEnemy <- function(b, range) {
	local best = null
	local bestD = range
	foreach (p in SigfPlayers()) {
		if (p.GetTeam() == b.GetTeam()) continue
		local d = (p.GetOrigin() - b.GetOrigin()).Length()
		if (d >= bestD) continue
		local tr = ::PG_Trace(b.EyePosition(), p.EyePosition(), b)
		if (tr.hit && !(tr.enthit != null && tr.enthit == p) && tr.fraction < 0.95) continue
		best = p
		bestD = d
	}
	return best
}

::PG_BotThink <- function(now) {
	local host = GetListenServerHost()
	local bots = []
	foreach (p in SigfPlayers()) if (p != host && !(p.entindex() in ::PG.carry)) bots.append(p)
	if (!bots.len()) return
	local b = bots[RandomInt(0, bots.len() - 1)]
	local cr = ::PG_NearCube(b, 300.0, null) // any side: bots walk up to a cube and grab it
	local foe = ::PG_NearestEnemy(b, 900.0)
	if (cr != null && foe != null && RandomInt(0, 99) < 75) {
		local aim = foe.GetCenter() - b.EyePosition()
		aim.Norm()
		cr.ent.SetAbsOrigin(b.EyePosition() + aim * 84.0)
		if (::PG_Grab(b, aim, aim)) {
			SigfIn(1.1, function() {
				if (!b.IsValid() || !b.IsAlive() || !foe.IsValid()) { if (b.IsValid()) ::PG_Drop(b); return }
				local d = foe.GetCenter() - b.EyePosition()
				d.Norm()
				::PG_Throw(b, d)
			})
		}
		return
	}
	if (RandomInt(0, 99) < 85) {
		local other = bots[RandomInt(0, bots.len() - 1)]
		if (::PG_BotShot(b.entindex(), b, 0)) {
			SigfIn(1.0, function() { if (other.IsValid() && other.IsAlive()) ::PG_BotShot(b.entindex(), other, 1) })
		}
	}
}

// ---- the player's gun ----

::PG_IsGun <- function(w) {
	if (w == null || !w.IsValid()) return false
	try { return w.GetSlot() == 2 } catch (e) { return false }
}

::PG_EquipGun <- function(h) {
	for (local i = 0; i < 8; i++) {
		local w = NetProps.GetPropEntityArray(h, "m_hMyWeapons", i)
		if (::PG_IsGun(w)) { h.Weapon_Switch(w); return }
	}
}

// While the Portal Gun slot is active, the stock melee weapon is hidden and the gun's picture is held in the right hand.
::PG_ShowGun <- function(h, aw) {
	aw.DisableDraw()
	local vis = ::PG.gunGlow
	if (vis != null && vis.IsValid()) return
	::PG.gunGlow = SpawnEntityFromTable("env_sprite", { model = "sigf/portal_gun.vmt", origin = h.GetOrigin(), scale = 0.15, rendermode = 2, spawnflags = 1 })
	::PG_GunFollow(h)
}

// the picture follows the right hand
::PG_GunFollow <- function(h) {
	local g = ::PG.gunGlow
	if (g == null || !g.IsValid()) return
	local id = h.LookupAttachment("effect_hand_R")
	g.SetAbsOrigin(id > 0 ? h.GetAttachmentOrigin(id) : h.GetOrigin() + Vector(0, 0, 50))
}

::PG_HideGun <- function() {
	local g = ::PG.gunGlow
	if (g != null && g.IsValid()) g.Kill()
	::PG.gunGlow = null
}


::PG_HostTick <- function(now) {
	local h = GetListenServerHost()
	if (h == null || !h.IsValid() || !h.IsAlive() || h.GetTeam() < 2) { ::PG.btn = 0; ::PG_HideGun(); return }
	local aw = h.GetActiveWeapon()
	if (::SigfDemoMode && !::PG_IsGun(aw) && now > ::PG.nextEquip) { ::PG.nextEquip = now + 1.0; ::PG_EquipGun(h); return }
	if (!::PG_IsGun(aw)) { ::PG.btn = 0; ::PG_HideGun(); return }
	if (now > ::PG.nextGun) {
		::PG.nextGun = now + 1.0
		::PG_ShowGun(h, aw)
	}
	::PG_GunFollow(h)
	NetProps.SetPropFloat(aw, "m_flNextPrimaryAttack", now + 0.3)
	NetProps.SetPropFloat(aw, "m_flNextSecondaryAttack", now + 0.3)
	if (::PG.scripted) return
	local btn = NetProps.GetPropInt(h, "m_nButtons")
	local edge = btn & ~::PG.btn
	::PG.btn = btn
	local key = h.entindex()
	local eye = h.EyePosition()
	local fwd = h.EyeAngles().Forward()
	if (edge & 1) {
		if (key in ::PG.carry) ::PG_Throw(h, fwd)
		else ::PG_Fire(key, h, eye, fwd, 0, 0.0)
	}
	if (edge & 2048) ::PG_Fire(key, h, eye, fwd, 1, 0.0)
	if (edge & 8192) ::PG_Grab(h, null, null)
}

// ---- main loop ----

::PG_Tick <- function() {
	local now = Time()
	::PG_HostTick(now)
	::PG_CarryTick()
	::PG_Transit(now)
	::PG_HitTick(now)
	if (now > ::PG.nextSlow) {
		::PG.nextSlow = now + 1.0
		::PG_CubeUpkeep(now)
		if (::PG.lv.len() > 300) ::PG.lv = {}
		if (now > ::PG.nextBot) { ::PG.nextBot = now + 2.5; ::PG_BotThink(now) }
	}
}

::PG_Think <- function() {
	try { ::PG_Tick() } catch (e) { printl("SIGF_ERROR portal tick: " + e) }
	return 0.03
}

::PG_Boot <- function() {
	foreach (m in [::PG_CUBE,"sigf/orb_blue.vmt", "sigf/orb_orange.vmt", "sigf/portal_gun.vmt", "sigf/portal_blue.vmt", "sigf/portal_orange.vmt", "sigf/cube_face.vmt"]) PrecacheModel(m)
	foreach (s in ["pg_blue", "pg_orange", "pg_fizzle", "portal_open", "portal_pass", "cube_hit", "cube_grab"]) PrecacheSound("sigf/" + s + ".wav")
	if (Entities.FindByName(null, "pg_clock") != null) return
	local c = SpawnEntityFromTable("info_target", { targetname = "pg_clock" })
	c.ValidateScriptScope()
	c.GetScriptScope().Think <- ::PG_Think
	AddThinkToEnt(c, "Think")
}

::PGEvents <- {
	OnGameEvent_teamplay_round_start = function(params) {
		::PG.portals = []
		::PG.cubes = []
		::PG.carry = {}
		::PG.lv = {}
		::PG.cool = {}
		SigfIn(0.5, function() { ::PG_Boot() })
	}
}
__CollectGameEventCallbacks(::PGEvents)

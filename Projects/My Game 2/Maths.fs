namespace MyGame2
open System.Numerics

[<RequireQualifiedAccess>]
module Maths =

    /// Catmull–Rom spline between p1 and p2 using p0 and p3 as neighbours.
    /// t in [0,1].
    let private catmullRom (p0:Vector3) (p1:Vector3) (p2:Vector3) (p3:Vector3) (t:single) =
        let t2 = t * t
        let t3 = t2 * t
        0.5f * (
            (2.0f * p1) +
            (-p0 + p2) * t +
            (2.0f * p0 - 5.0f * p1 + 4.0f * p2 - p3) * t2 +
            (-p0 + 3.0f * p1 - 3.0f * p2 + p3) * t3)

    /// Given control points p[0..n-1] (n >= 2), evaluate spline at global t in [0,1].
    let evalSpline (points:Vector3 array) (tGlobal:single) =
        let n = points.Length

        if n = 1 
        then points.[0]
        else

        // map tGlobal into segment index and local t
        let segCount = n - 1
        let tClamped = max 0.0f (min 1.0f tGlobal)
        let f = tClamped * single segCount
        let seg = int (floor (float f))
        let seg = if seg >= segCount then segCount - 1 else seg
        let t = f - single seg

        let p1 = points.[seg]
        let p2 = points.[seg+1]
        let p0 = if seg = 0 then p1 else points.[seg-1]
        let p3 = if seg+2 >= n then p2 else points.[seg+2]
        catmullRom p0 p1 p2 p3 t

    /// Approximate curve length by sampling.
    let approxSplineLength (points:Vector3 array) (samplesPerSegment:int) =
        let n = points.Length

        if n < 2 
        then 0.0f 
        else

        let segCount = n - 1
        let totalSamples = segCount * samplesPerSegment
        let mutable prev = evalSpline points 0.0f
        let mutable acc = 0.0f
        for i = 1 to totalSamples do
            let t = single i / single totalSamples
            let p = evalSpline points t
            acc <- acc + Vector3.Distance(prev, p)
            prev <- p
        acc

    let lookRotation (dir: Vector3) =
        let dirNorm =
            if dir.LengthSquared() < 1e-6f 
            then Vector3.UnitZ
            else Vector3.Normalize dir
        let up = Vector3.UnitY
        // model faces opposite direction → use -dirNorm (as we already established)
        let m = Matrix4x4.CreateWorld(Vector3.Zero, dirNorm, up)
        Quaternion.CreateFromRotationMatrix m

    let closestPointOnSegment (a : Vector3) (b : Vector3) (p : Vector3) =
        let ab = b - a
        let abLenSq = ab.LengthSquared ()
        if abLenSq <= 1e-8f then a else
        let t = Vector3.Dot(p - a, ab) / abLenSq
        let t = Math.Clamp(t, 0f, 1f)
        a + ab * t

    let tryClosestPointOnPolyline (points : Vector3 list) (p : Vector3) =
        match points with
        | [] -> None
        | [point] -> Some point
        | _ ->
        let rec go bestPoint bestDistSq pts =
            match pts with
            | a :: (b :: _ as tail) ->
                let cp = closestPointOnSegment a b p
                let d2 = Vector3.DistanceSquared (cp, p)
                if d2 < bestDistSq then go cp d2 tail
                else go bestPoint bestDistSq tail
            | _ -> (bestPoint, bestDistSq)
        let a0 = points.Head
        let b0 = points.Tail.Head
        let cp0 = closestPointOnSegment a0 b0 p
        let d20 = Vector3.DistanceSquared (cp0, p)
        let (best, _) = go cp0 d20 points
        Some best

    /// Ensure quaternion q2 is in the same hemisphere as q1 (shortest path)
    /// Only aligns if the angle between them is less than ~170 degrees
    /// to allow intentional long rotations (like 360° spins)
    let private alignQuaternion (q1: Quaternion) (q2: Quaternion) =
        let dot = Quaternion.Dot(q1, q2)
        // Only flip if dot is negative AND the angle is small enough
        // dot < 0 means angle > 90°, dot < -0.17 means angle > ~170°
        // We allow angles up to ~170° to preserve intentional long rotations
        if dot < -0.17f then
            Quaternion(-q2.X, -q2.Y, -q2.Z, -q2.W)
        else
            q2

    /// Align an array of quaternions so they all take shortest paths
    /// (unless the rotation is intentionally large)
    let private alignQuaternions (rotations: Quaternion array) =
        if rotations.Length <= 1 then rotations
        else
        let aligned = Array.copy rotations
        for i = 1 to aligned.Length - 1 do
            aligned.[i] <- alignQuaternion aligned.[i - 1] aligned.[i]
        aligned

    /// Compute the logarithm of a unit quaternion
    let private quatLog (q: Quaternion) =
        // Ensure q.W is in valid range for acos
        let w = max -1.0f (min 1.0f q.W)
        let v = Vector3(q.X, q.Y, q.Z)
        let vLen = v.Length()
        if vLen < 1e-6f then Vector3.Zero
        else
        let theta = acos w
        (theta / vLen) * v

    /// Compute the exponential of a vector (as quaternion)
    let private quatExp (v: Vector3) =
        let theta = v.Length()
        if theta < 1e-6f then Quaternion.Identity
        else
        let axis = v / theta
        let sinTheta = sin theta
        let cosTheta = cos theta
        Quaternion(axis.X * sinTheta, axis.Y * sinTheta, axis.Z * sinTheta, cosTheta)

    /// Compute SQUAD intermediate control point for quaternion q_i
    /// given neighbors q_{i-1} and q_{i+1}. All inputs should be pre-aligned.
    let private squadControlPoint (qPrev: Quaternion) (q: Quaternion) (qNext: Quaternion) =
        let qInv = Quaternion.Inverse q
        let logPrev = quatLog (Quaternion.Normalize (qInv * qPrev))
        let logNext = quatLog (Quaternion.Normalize (qInv * qNext))
        let avg = -0.25f * (logPrev + logNext)
        Quaternion.Normalize (q * quatExp avg)

    /// Slerp that does NOT auto-flip to shortest path.
    /// Use this when quaternions are pre-aligned and we want to respect that alignment.
    let private slerpDirect (q1: Quaternion) (q2: Quaternion) (t: single) =
        let dot = Quaternion.Dot(q1, q2)
        // Clamp dot to valid range for acos
        let dotClamped = max -1.0f (min 1.0f dot)
        
        if abs dotClamped > 0.9995f then
            // Very close quaternions - use normalized lerp to avoid numerical issues
            Quaternion.Normalize(Quaternion.Lerp(q1, q2, t))
        else
            let theta = acos dotClamped
            let sinTheta = sin theta
            let w1 = sin((1.0f - t) * theta) / sinTheta
            let w2 = sin(t * theta) / sinTheta
            Quaternion(
                w1 * q1.X + w2 * q2.X,
                w1 * q1.Y + w2 * q2.Y,
                w1 * q1.Z + w2 * q2.Z,
                w1 * q1.W + w2 * q2.W
            )

    /// SQUAD interpolation between q1 and q2 with control points s1 and s2
    let private squad (q1: Quaternion) (q2: Quaternion) (s1: Quaternion) (s2: Quaternion) (t: single) =
        // Align control points with their respective quaternions
        let s1Aligned = if Quaternion.Dot(q1, s1) < 0.0f then Quaternion(-s1.X, -s1.Y, -s1.Z, -s1.W) else s1
        let s2Aligned = if Quaternion.Dot(q2, s2) < 0.0f then Quaternion(-s2.X, -s2.Y, -s2.Z, -s2.W) else s2
        
        // Use our custom slerp that respects alignment
        let slerpQ = slerpDirect q1 q2 t
        let slerpS = slerpDirect s1Aligned s2Aligned t
        
        // Align slerpS with slerpQ before final interpolation
        let slerpSAligned = if Quaternion.Dot(slerpQ, slerpS) < 0.0f then Quaternion(-slerpS.X, -slerpS.Y, -slerpS.Z, -slerpS.W) else slerpS
        slerpDirect slerpQ slerpSAligned (2.0f * t * (1.0f - t))

    /// Pre-align entire quaternion array to ensure consistent rotation direction.
    /// Each quaternion is aligned to its predecessor to form a consistent chain.
    let private preAlignQuaternions (rotations: Quaternion array) =
        let n = rotations.Length
        if n <= 1 then rotations
        else
        let aligned = Array.zeroCreate n
        aligned.[0] <- rotations.[0]
        for i = 1 to n - 1 do
            let prev = aligned.[i - 1]
            let curr = rotations.[i]
            aligned.[i] <- 
                if Quaternion.Dot(prev, curr) < 0.0f 
                then Quaternion(-curr.X, -curr.Y, -curr.Z, -curr.W) 
                else curr
        aligned

    /// Interpolate through multiple quaternion rotations using SQUAD.
    /// Provides C1 continuous (smooth angular velocity) interpolation.
    /// For 360° rotations, ensure waypoints are spaced less than 180° apart.
    /// t in [0,1] maps across all segments.
    let slerpAlongPath (rotations: Quaternion array) (t: single) =
        let n = rotations.Length
        if n = 0 then Quaternion.Identity
        elif n = 1 then rotations.[0] 
        else
        
        // Pre-align entire array to ensure consistent rotation direction across all segments
        let aligned = preAlignQuaternions rotations
        
        let tClamped = max 0.0f (min 1.0f t)
        
        if n = 2 then 
            // Use our custom slerp that respects alignment
            slerpDirect aligned.[0] aligned.[1] tClamped
        else
        
        let scaledT = tClamped * single (n - 1)
        let segment = int scaledT |> min (n - 2)
        let localT = scaledT - single segment

        // Get the four quaternions needed for SQUAD (with boundary handling)
        // Using pre-aligned array ensures consistency across segment transitions
        let q0 = if segment = 0 then aligned.[0] else aligned.[segment - 1]
        let q1 = aligned.[segment]
        let q2 = aligned.[segment + 1]
        let q3 = if segment + 2 >= n then aligned.[n - 1] else aligned.[segment + 2]

        // Compute SQUAD control points
        let s1 = squadControlPoint q0 q1 q2
        let s2 = squadControlPoint q1 q2 q3

        // SQUAD interpolation for smooth transitions
        squad q1 q2 s1 s2 localT


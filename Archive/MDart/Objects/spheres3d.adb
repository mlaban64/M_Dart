--  package Spheres3D body

with Ada.Text_IO; use Ada.Text_IO;
with LARGE_FLOAT_Functions;

package body Spheres3D is

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructSphere3D (ObjName : in Unbounded_String) return Object3D_Ptr is
      newObj : Object3D_Ptr;
   begin
      newObj := new Sphere3D;
      newObj.all.SetObj3DName (ObjName);
      return newObj;
   end ConstructSphere3D;

   ---------------------------------------
   --  CLASS-WIDE SPHERE3D SUBPROGRAMS  --
   ---------------------------------

   --  Intersect a unit Sphere3D with a Ray
   overriding procedure Intersect
     (Obj3D   : in Sphere3D;
      Ray     : in Ray3D;
      HP_List : in out HitPoint3D_List)
   is
      A, B, C, D, E, lambda : LARGE_FLOAT;
      lOrg                  : Point3D;
      lDir                  : Vector3D;
      lNorm                 : Normal3D;
      lPoint                : Point3D;
      lInv_Trn              : Matrix3D;
      newHP                 : HitPoint3D_Ptr;
   begin
      --  First, Transform the Ray into Object Coordinates, by applying the inverse Transformation
      --  Matrix of the object to the Ray. Do NOT normalize...
      lInv_Trn := Obj3D.GetObj3DTrans_Inv;
      lDir     := lInv_Trn * GetRay3DDirection (Ray);
      lOrg     := lInv_Trn * GetRay3DOrigin (Ray);

      --  Now intersect
      --  Calculate the factors of the quadratic formula
      A := Length_Squared (lDir);
      B := 2.0 *
           (GetX (lOrg) * GetX (lDir) +
            GetY (lOrg) * GetY (lDir) +
            GetZ (lOrg) * GetZ (lDir));
      C := Distance_Squared_Org (lOrg) - 1.0;

      --  Calculate the discriminant. If >= 0, then we have a hit or two
      D := B * B - 4.0 * A * C;
      --  We have one hit
      if D = 0.0 then
         lambda := -B / (2.0 * A);
         if lambda > 0.0 then
            newHP := new HitPoint3D;
            SetHitPoint3DLambda (lambda, newHP);
            lPoint := GetPoint3DOnOrgDir (lambda, lOrg, lDir);
            SetHitPoint3DLocalHP (lPoint, newHP);
            lNorm := ConstructNormal3D (GetX (lPoint), GetY (lPoint), GetZ (lPoint));
            SetHitPoint3DLocalNV (lNorm, newHP);
            SetHitPoint3DObject (Obj3D, newHP);
            AddHitPoint3DToHitPoint3DList (newHP, HP_List);
         end if;
      end if;
      --  We have two hits
      if D > 0.0 then
         E := 1.0 / (2.0 * A);
         --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind
         --  us, and no point in determining the other root...that will be behind us as well
         lambda := E * (-B + LARGE_FLOAT_Functions.Sqrt (D));
         if lambda > 0.0 then
            newHP := new HitPoint3D;
            SetHitPoint3DLambda (lambda, newHP);
            lPoint := GetPoint3DOnOrgDir (lambda, lOrg, lDir);
            SetHitPoint3DLocalHP (lPoint, newHP);
            lNorm := ConstructNormal3D (GetX (lPoint), GetY (lPoint), GetZ (lPoint));
            SetHitPoint3DLocalNV (lNorm, newHP);
            SetHitPoint3DObject (Obj3D, newHP);
            AddHitPoint3DToHitPoint3DList (newHP, HP_List);
            --  The second root
            lambda := E * (-B - LARGE_FLOAT_Functions.Sqrt (D));
            if lambda > 0.0 then
               newHP := new HitPoint3D;
               SetHitPoint3DLambda (lambda, newHP);
               lPoint := GetPoint3DOnOrgDir (lambda, lOrg, lDir);
               SetHitPoint3DLocalHP (lPoint, newHP);
               lNorm := ConstructNormal3D (GetX (lPoint), GetY (lPoint), GetZ (lPoint));
               SetHitPoint3DLocalNV (lNorm, newHP);
               SetHitPoint3DObject (Obj3D, newHP);
               AddHitPoint3DToHitPoint3DList (newHP, HP_List);
            end if;
         end if;
      end if;

   end Intersect;

end Spheres3D;

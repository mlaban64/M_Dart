--  package Cylinders3D body

with Ada.Text_IO; use Ada.Text_IO;
with LARGE_FLOAT_Functions;

package body Cylinders3D is

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructCylinder3D (ObjName : in Unbounded_String) return Object3D_Ptr is
      newObj : Object3D_Ptr;
   begin
      newObj := new Cylinder3D;
      newObj.all.SetObj3DName (ObjName);
      return newObj;
   end ConstructCylinder3D;

   ---------------------------------------
   --  CLASS-WIDE SPHERE3D SUBPROGRAMS  --
   ---------------------------------

   --  Intersect a unit Cylinder3D with a Ray
   overriding procedure Intersect
     (Obj3D   : in Cylinder3D;
      Ray     : in Ray3D;
      HP_List : in out HitPoint3D_List)
   is
      A, B, C, D, Dsr, E, lambda : LARGE_FLOAT;
      dX, dY, dZ                 : LARGE_FLOAT;
      oX, oY, oZ                 : LARGE_FLOAT;
      lX, lY, lZ                 : LARGE_FLOAT;
      oXs, oYs, oZs              : LARGE_FLOAT;
      tYmin, tYmax               : LARGE_FLOAT;
      yNear, yFar                : LARGE_FLOAT;
      lOrg                       : Point3D;
      lDir                       : Vector3D;
      lNorm                      : Normal3D;
      lP_far, lP_near            : Point3D;
      lInv_Trn                   : Matrix3D;
      newHP                      : HitPoint3D_Ptr;
   begin
      --  First, Transform the Ray into Object Coordinates, by applying the inverse Transformation
      --  Matrix of the object to the Ray. Do NOT normalize...
      lInv_Trn := Obj3D.GetObj3DTrans_Inv;
      lDir     := lInv_Trn * GetRay3DDirection (Ray);
      lOrg     := lInv_Trn * GetRay3DOrigin (Ray);

      --  Set the starting conditions
      dX  := GetX (lDir);
      dY  := GetY (lDir);
      dZ  := GetZ (lDir);
      oX  := GetX (lOrg);
      oY  := GetY (lOrg);
      oZ  := GetZ (lOrg);
      oXs := oX * oX;
      oZs := oZ * oZ;

      --  First, check if we are inside the infinite cylinder and fully
      --  vertical. If so, we may only hit the end caps
      if dX = 0.0 and dZ = 0.0 and (oXs + oZs) < 1.0 then
         Put_Line("*** Vertical Ray in Cylinder3D.Intersect");
         --  inside the cylinder?
         if oY < 1.0 and oY > -1.0 then
            null;
         -- above the 1.0 end cap and pointing down?
         elsif oY > 1.0 and dY < 0.0 then
            newHP := new HitPoint3D;
            SetHitPoint3DLambda (oY - 1.0, newHP);
            SetHitPoint3DLocalHP (ConstructPoint3D(oX, 1.0, oZ), newHP);
            lNorm := ConstructNormal3D (0.0, 1.0, 0.0);
            SetHitPoint3DLocalNV (lNorm, newHP);
            AddHitPoint3DToHitPoint3DList (newHP, HP_List);
            newHP := new HitPoint3D;
            SetHitPoint3DLambda (oY + 1.0, newHP);
            SetHitPoint3DLocalHP (ConstructPoint3D(oX, -1.0, oZ), newHP);
            lNorm := ConstructNormal3D (0.0, -1.0, 0.0);
            SetHitPoint3DLocalNV (lNorm, newHP);
            AddHitPoint3DToHitPoint3DList (newHP, HP_List);
         elsif oY < -1.0 and dY > 0.0 then
            null;
         end if;

      else --  Not vertical and inside, so full intersection calculation

         --  Now intersect with an infinite cylinder
         --  First, calculate the factors of the quadratic formula
         A := dX * dX + dZ * dZ;
         B := 2.0 * (oX * dX + oZ * dZ);
         C := oX * oX + oZ * oZ - 1.0;
         --  Calculate the discriminant. If D > 0, then we have two hits
         D := B * B - 4.0 * A * C;
         if D > 0.0 then
            E := 1.0 / (2.0 * A);
            --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind
            --  us, and no point in determining the other root...that will be behind us as well
            Dsr   := LARGE_FLOAT_Functions.Sqrt (D);
            tYmax := E * (-B + Dsr);

            if tYmax > 0.0 then
               --  Calculate the far hitpoint
               lP_far := GetPoint3DOnOrgDir (tYmax, lOrg, lDir);
               yFar   := GetY (lP_far);
               --  Calculate the near hitpoint
               tYmin   := E * (-B - Dsr);
               lP_near := GetPoint3DOnOrgDir (tYmin, lOrg, lDir);
               yNear   := GetY (lP_near);

               --  Now check the various conditions
               --  If yFar is between -1 and 1, we exit the cylinder there
               if yFar > -1.0 and yFar < 1.0 then
                  Put_Line("*** EXIT in Cylinder3D.Intersect");
                  newHP := new HitPoint3D;
                  SetHitPoint3DLambda (tYmax, newHP);
                  SetHitPoint3DLocalHP (lP_far, newHP);
                  lNorm := ConstructNormal3D (GetX (lP_far), 0.0, GetZ (lP_far));
                  SetHitPoint3DLocalNV (lNorm, newHP);
                  AddHitPoint3DToHitPoint3DList (newHP, HP_List);
               end if;
               --  If yNear is between -1 and 1 and tYmin > 0, we enter the cylinder there
               if yNear > -1.0 and yNear < 1.0 and tYmin > 0.0 then
                  Put_Line("*** ENTER in Cylinder3D.Intersect");
                  newHP := new HitPoint3D;
                  SetHitPoint3DLambda (tYmin, newHP);
                  SetHitPoint3DLocalHP (lP_near, newHP);
                  lNorm := ConstructNormal3D (GetX (lP_near), 0.0, GetZ (lP_near));
                  SetHitPoint3DLocalNV (lNorm, newHP);
                  AddHitPoint3DToHitPoint3DList (newHP, HP_List);
               end if;

               --  if yNear > 1, that point is on the infinite cylinder above the
               --  1 end cap, but we still may hit the end cap if yFar < 1
               if yNear > 1.0 and yFar < 1.0 then
                  tYmin := (1.0 - oY) / dY;
                  if tYmin > 0.0 then --  we have a hit at the end cap
                     Put_Line("*** ENDCAP 1:1 in Cylinder3D.Intersect");
                     lP_near := GetPoint3DOnOrgDir (tYmin, lOrg, lDir);
                     newHP   := new HitPoint3D;
                     SetHitPoint3DLambda (tYmin, newHP);
                     SetHitPoint3DLocalHP (lP_near, newHP);
                     lNorm := ConstructNormal3D (GetX (lP_near), 0.0, GetZ (lP_near));
                     SetHitPoint3DLocalNV (lNorm, newHP);
                     AddHitPoint3DToHitPoint3DList (newHP, HP_List);
                  end if;
               end if;

               --  if yNear inside [-1,1], that point is on the infinite cylinder above the
               --  -1 end cap, but we still may hit the end cap if yFar < -1
               if yNear > -1.0 and yNear < 1.0 and yFar < -1.0 then
                  tYmax := (-1.0 - oY) / dY;
                  if tYmax > 0.0 then --  we have a hit at the end cap
                     Put_Line("*** ENDCAP 1:-1 in Cylinder3D.Intersect");
                     lP_far := GetPoint3DOnOrgDir (tYmax, lOrg, lDir);
                     newHP  := new HitPoint3D;
                     SetHitPoint3DLambda (tYmax, newHP);
                     SetHitPoint3DLocalHP (lP_far, newHP);
                     lNorm := ConstructNormal3D (GetX (lP_far), 0.0, GetZ (lP_far));
                     SetHitPoint3DLocalNV (lNorm, newHP);
                     AddHitPoint3DToHitPoint3DList (newHP, HP_List);
                  end if;
               end if;
               --  if yFar > 1, that point is on the infinite cylinder above the
               --  1 end cap, but we still may hit the end cap if yNear < 1
               if yFar > 1.0 and yNear < 1.0 then
                  tYmin := (1.0 - oY) / dY;
                  if tYmin > 0.0 then --  we have a hit at the end cap
                     lP_near := GetPoint3DOnOrgDir (tYmin, lOrg, lDir);
                     newHP   := new HitPoint3D;
                     SetHitPoint3DLambda (tYmax, newHP);
                     SetHitPoint3DLocalHP (lP_far, newHP);
                     lNorm := ConstructNormal3D (GetX (lP_far), 0.0, GetZ (lP_far));
                     SetHitPoint3DLocalNV (lNorm, newHP);
                     AddHitPoint3DToHitPoint3DList (newHP, HP_List);
                  end if;
               end if;

               --  if yNear > -1, that point is on the infinite cylinder above the
               --  -1 end cap, but we still may hit the end cap if yFar < -1
               if yNear > -1.0 and yFar < -1.0 then
                  tYmax := (-1.0 - oY) / dY;
                  if tYmax > 0.0 then --  we have a hit at the end cap
                     lP_far  := GetPoint3DOnOrgDir (tYmax, lOrg, lDir);
                     lP_near := GetPoint3DOnOrgDir (tYmin, lOrg, lDir);
                     newHP   := new HitPoint3D;
                     SetHitPoint3DLambda (tYmin, newHP);
                     SetHitPoint3DLocalHP (lP_near, newHP);
                     lNorm := ConstructNormal3D (GetX (lP_near), 0.0, GetZ (lP_near));
                     SetHitPoint3DLocalNV (lNorm, newHP);
                     AddHitPoint3DToHitPoint3DList (newHP, HP_List);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Intersect;

end Cylinders3D;
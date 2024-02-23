--  package Cubes3D body

with Ada.Text_IO; use Ada.Text_IO;

package body Cubes3D is

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructCube3D (ObjName : in Unbounded_String) return Object3D_Ptr is
      newObj : Object3D_Ptr;
   begin
      newObj := new Cube3D;
      newObj.all.SetObj3DName (ObjName);
      return newObj;
   end ConstructCube3D;

   ---------------------------------------
   --  CLASS-WIDE Cube3D SUBPROGRAMS  --
   ---------------------------------

   --  Intersect a Cube3D with a Ray
   overriding procedure Intersect
     (Obj3D   : in Cube3D;
      Ray     : in Ray3D;
      HP_List : in out HitPoint3D_List)
   is
      tXmin, tXmax      : LARGE_FLOAT;
      tYmin, tYmax      : LARGE_FLOAT;
      tZmin, tZmax      : LARGE_FLOAT;
      Xo, Yo, Zo        : LARGE_FLOAT;
      Xd, Yd, Zd        : LARGE_FLOAT;
      t0, t1, tt        : LARGE_FLOAT;
      hit               : Boolean;
      lOrg              : Point3D;
      lDir              : Vector3D;
      lNorm             : Normal3D;
      face_in, face_out : Integer;
      lPoint            : Point3D;
      lInv_Trn          : Matrix3D;
      newHP             : HitPoint3D_Ptr;
   begin
      --  First, Transform the Ray into Object Coordinates, by applying the inverse Transformation
      --  Matrix of the object to the Ray. Do NOT normalize...
      lInv_Trn := Obj3D.GetObj3DTrans_Inv;
      lDir     := lInv_Trn * GetRay3DDirection (Ray);
      lOrg     := lInv_Trn * GetRay3DOrigin (Ray);

      --  Set the starting conditions
      tXmin := LARGE_FLOAT'First;
      tXmax := LARGE_FLOAT'Last;
      tYmin := LARGE_FLOAT'First;
      tYmax := LARGE_FLOAT'Last;
      tZmin := LARGE_FLOAT'First;
      tZmax := LARGE_FLOAT'Last;
      t0    := LARGE_FLOAT'First;
      t1    := LARGE_FLOAT'Last;
      hit   := True;

      --  Now go through all slabs
      --  Process the X-slab. First, test if we have a ray parallel to the slabs
      Xd := GetX (lDir);
      Xo := GetX (lOrg);
      if Xd = 0.0 then --  parallel
         if Xo < -1.0 or Xo > 1.0 then --  Origin not between slabs and parallel. No hit
            hit := False;
         end if;
      else --  Not parallel
         tt := 1.0 / Xd; --  To avoid two devisions by Xd, we multiply with 1/Xd
         if tt < 0.0 then
            tXmin := (1.0 - Xo) * tt;
            tXmax := (-1.0 - Xo) * tt;
         else
            tXmin := (-1.0 - Xo) * tt;
            tXmax := (1.0 - Xo) * tt;
         end if;
      end if;

      --  Check if we should go on...
      if hit then
         --  Process the Y-slab. First, test if we have a ray parallel to the slabs
         Yd := GetY (lDir);
         Yo := GetY (lOrg);
         if Yd = 0.0 then --  parallel
            if Yo < -1.0 or Yo > 1.0 then --  Origin not between slabs and parallel. No hit
               hit := False;
            end if;
         else --  Not parallel
            tt := 1.0 / Yd; --  To avoid two devisions by Yd, we multiply with 1/Yd
            if tt < 0.0 then
               tYmin := (1.0 - Yo) * tt;
               tYmax := (-1.0 - Yo) * tt;
            else
               tYmin := (-1.0 - Yo) * tt;
               tYmax := (1.0 - Yo) * tt;
            end if;
         end if;

         --  Check if we should go on...
         if hit then
            --  Process the Z-slab. First, test if we have a ray parallel to the slabs
            Zd := GetZ (lDir);
            Zo := GetZ (lOrg);
            if Zd = 0.0 then --  parallel
               if Zo < -1.0 or Zo > 1.0 then --  Origin not between slabs and parallel. No hit
                  hit := False;
               end if;
            else --  Not parallel
               tt := 1.0 / Zd; --  To avoid two devisions by Zd, we multiply with 1/Zd
               if tt < 0.0 then
                  tZmin := (1.0 - Zo) * tt;
                  tZmax := (-1.0 - Zo) * tt;
               else
                  tZmin := (-1.0 - Zo) * tt;
                  tZmax := (1.0 - Zo) * tt;
               end if;
            end if;

            --  Now get the largest entering value t0
            if tXmin > tYmin then
               t0 := tXmin;
               if Xd < 0.0 then
                  face_in := 3; --  +x face
               else
                  face_in := 0; --  -x face
               end if;
            else
               t0 := tYmin;
               if Yd < 0.0 then
                  face_in := 4; --  +y face
               else
                  face_in := 1; --  -y face
               end if;
            end if;
            if tZmin > t0 then
               t0 := tZmin;
               if Zd < 0.0 then
                  face_in := 5; --  +z face
               else
                  face_in := 2; --  -z face
               end if;
            end if;

            --  Now get the smallest exiting value t1
            if tXmax < tYmax then
               t1 := tXmax;
               if Xd < 0.0 then
                  face_out := 0; --  -x face
               else
                  face_out := 3; --  +x face
               end if;
            else
               t1 := tYmax;
               if Yd < 0.0 then
                  face_out := 1; --  -y face
               else
                  face_out := 4; --  +y face
               end if;
            end if;
            if tZmax < t1 then
               t1 := tZmax;
               if Zd < 0.0 then
                  face_out := 2; --  -z face
               else
                  face_out := 5; --  +z face
               end if;
            end if;

            --  hit conditions: t0 before t1 and t1 in front of us
            if (t0 < t1) and t1 > 0.0 then
               if t0 > 0.0 then --  both hitpoint in front of us
                  --  first the far hitpoint
                  newHP := new HitPoint3D;
                  SetHitPoint3DLambda (t1, newHP);
                  lPoint := GetPoint3DOnOrgDir (t1, lOrg, lDir);
                  SetHitPoint3DLocalHP (lPoint, newHP);
                  lNorm := GetNormalCube3D (face_out);
                  SetHitPoint3DLocalNV (lNorm, newHP);
                  SetHitPoint3DObject (Obj3D, newHP);
                  AddHitPoint3DToHitPoint3DList (newHP, HP_List);
                  --  then the near hitpoint
                  newHP := new HitPoint3D;
                  SetHitPoint3DLambda (t0, newHP);
                  lPoint := GetPoint3DOnOrgDir (t0, lOrg, lDir);
                  SetHitPoint3DLocalHP (lPoint, newHP);
                  lNorm := GetNormalCube3D (face_in);
                  SetHitPoint3DLocalNV (lNorm, newHP);
                  SetHitPoint3DObject (Obj3D, newHP);
                  AddHitPoint3DToHitPoint3DList (newHP, HP_List);
               else
                  --  we're inside the cube. Only the far hitpoint applies
                  --  but the normal must be inversed
                  newHP := new HitPoint3D;
                  SetHitPoint3DLambda (t1, newHP);
                  lPoint := GetPoint3DOnOrgDir (t1, lOrg, lDir);
                  SetHitPoint3DLocalHP (lPoint, newHP);
                  lNorm := -GetNormalCube3D (face_out);
                  SetHitPoint3DLocalNV (lNorm, newHP);
                  SetHitPoint3DObject (Obj3D, newHP);
                  AddHitPoint3DToHitPoint3DList (newHP, HP_List);
               end if;
            end if;
         end if;
      end if;

   end Intersect;

   --  Return the normal for a Cube3D for a face number
   function GetNormalCube3D (face : Integer) return Normal3D is
   begin
      case face is
         when 0 =>
            return ConstructNormal3D (-1.0, 0.0, 0.0);
         when 1 =>
            return ConstructNormal3D (0.0, -1.0, 0.0);
         when 2 =>
            return ConstructNormal3D (0.0, 0.0, -1.0);
         when 3 =>
            return ConstructNormal3D (1.0, 0.0, 0.0);
         when 4 =>
            return ConstructNormal3D (0.0, 1.0, 0.0);
         when 5 =>
            return ConstructNormal3D (0.0, 0.0, 1.0);
         when others =>
            Put_Line ("*** Illegal CASE value in GetNormalCube3D");
            return ConstructNormal3D (1.0, 0.0, 0.0);
      end case;
   end GetNormalCube3D;

end Cubes3D;

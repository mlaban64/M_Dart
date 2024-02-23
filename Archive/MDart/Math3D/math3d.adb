--  Package Math3D body
with Ada.Text_IO; use Ada.Text_IO;
with LARGE_FLOAT_Functions;

package body Math3D is

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   --  Print a Point2D record
   procedure Put (Point : in Point2D) is
   begin
      Put ("Point2D = { ");
      Put (Point.x);
      Put (" , ");
      Put (Point.y);
      Put (" }");
      New_Line;
   end Put;

   --  Print a Point3D record
   procedure Put (Point : in Point3D) is
   begin
      Put ("Point3D = { ");
      Put (Point.x);
      Put (" , ");
      Put (Point.y);
      Put (" , ");
      Put (Point.z);
      Put_Line (" }");
   end Put;

   --  Print a Vector3D record
   procedure Put (Vector : in Vector3D) is
   begin
      Put ("Vector3D = { ");
      Put (Vector.x);
      Put (" , ");
      Put (Vector.y);
      Put (" , ");
      Put (Vector.z);
      Put_Line (" }");
   end Put;

   --  Print a Normal3D record
   procedure Put (Normal : in Normal3D) is
   begin
      Put ("Normal3D = { ");
      Put (Normal.x);
      Put (" , ");
      Put (Normal.y);
      Put (" , ");
      Put (Normal.z);
      Put_Line (" }");
   end Put;

   --  Print a Matrix3D record
   procedure Put (Matrix : in Matrix3D) is
   begin
      Put ("Matrix3D = { ");
      Put (Matrix.x1);
      Put (" , ");
      Put (Matrix.x2);
      Put (" , ");
      Put (Matrix.x3);
      Put (" , ");
      Put (Matrix.tx);
      Put_Line (" }");
      Put ("             ");
      Put (Matrix.y1);
      Put (" , ");
      Put (Matrix.y2);
      Put (" , ");
      Put (Matrix.y3);
      Put (" , ");
      Put (Matrix.ty);
      Put_Line (" }");
      Put ("             ");
      Put (Matrix.z1);
      Put (" , ");
      Put (Matrix.z2);
      Put (" , ");
      Put (Matrix.z3);
      Put (" , ");
      Put (Matrix.tz);
      Put_Line (" }");
      Put ("             ");
      Put (LARGE_FLOAT (0.0));
      Put (" , ");
      Put (LARGE_FLOAT (0.0));
      Put (" , ");
      Put (LARGE_FLOAT (0.0));
      Put (" , ");
      Put (LARGE_FLOAT (1.0));
      Put_Line (" }");
   end Put;

   --  Print a Ray3D
   procedure Put (Ray : in Ray3D) is
   begin
      Put_Line ("BEGIN Ray3D");
      Put_Line ("ORIGIN = ");
      Put (Ray.Org);
      Put_Line ("DIRECTION = ");
      Put (Ray.Dir);
      Put_Line ("END Ray3D");
   end Put;

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   --  Construct a Point2D from 2 floats
   function ConstructPoint2D (x, y : in LARGE_FLOAT) return Point2D is
      Point : Point2D;
   begin
      Point.x := x;
      Point.y := y;
      return Point;
   end ConstructPoint2D;

   --  Construct a Point3D from 3 floats
   function ConstructPoint3D (x, y, z : in LARGE_FLOAT) return Point3D is
      Point : Point3D;
   begin
      Point.x := x;
      Point.y := y;
      Point.z := z;
      return Point;
   end ConstructPoint3D;

   --  Construct a Vector3D from 3 floats
   function ConstructVector3D (x, y, z : in LARGE_FLOAT) return Vector3D is
      Vector : Vector3D;
   begin
      Vector.x := x;
      Vector.y := y;
      Vector.z := z;
      return Vector;
   end ConstructVector3D;

   --  Construct a Normal3D from 3 floats
   function ConstructNormal3D (x, y, z : in LARGE_FLOAT) return Normal3D is
      Normal : Normal3D;
   begin
      Normal.x := x;
      Normal.y := y;
      Normal.z := z;
      return Normal;
   end ConstructNormal3D;

   --  Construct a Unit Matrix3D
   function ConstructUnitMatrix3D return Matrix3D is
      Matrix : Matrix3D;
   begin
      Matrix.x1 := 1.0;
      Matrix.y2 := 1.0;
      Matrix.z3 := 1.0;
      return Matrix;
   end ConstructUnitMatrix3D;

   --  Construct a Translation Matrix3D
   function ConstructTranslateMatrix3D (tx, ty, tz : in LARGE_FLOAT) return Matrix3D is
      Matrix : Matrix3D;
   begin
      Matrix.x1 := 1.0;
      Matrix.y2 := 1.0;
      Matrix.z3 := 1.0;
      Matrix.tx := tx;
      Matrix.ty := ty;
      Matrix.tz := tz;
      return Matrix;
   end ConstructTranslateMatrix3D;

   --  Function to construct a Scaling Matrix3D
   function ConstructScaleMatrix3D (sx, sy, sz : in LARGE_FLOAT) return Matrix3D is
      Result : Matrix3D;
   begin
      Result.x1 := sx;
      Result.y2 := sy;
      Result.z3 := sz;
      return Result;
   end ConstructScaleMatrix3D;

   --  Function to construct a Shearing Matrix3D
   function ConstructShearMatrix3D
     (Hxy, Hxz, Hyx, Hyz, Hzx, Hzy : in LARGE_FLOAT)
      return                         Matrix3D
   is
      Result : Matrix3D;
   begin
      Result.x1 := 1.0;
      Result.x2 := Hyx;
      Result.x3 := Hzx;
      Result.y1 := Hxy;
      Result.y2 := 1.0;
      Result.y3 := Hzy;
      Result.z1 := Hxz;
      Result.z2 := Hyz;
      Result.z3 := 1.0;
      return Result;
   end ConstructShearMatrix3D;

   --  Function to construct a Rotation Matrix3D around the X-axis
   function ConstructRotXMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D is
      Result : Matrix3D;
   begin
      Result.x1 := 1.0;
      Result.y2 := LARGE_FLOAT_Functions.Cos (Angle);
      Result.y3 := -LARGE_FLOAT_Functions.Sin (Angle);
      Result.z2 := LARGE_FLOAT_Functions.Sin (Angle);
      Result.z3 := LARGE_FLOAT_Functions.Cos (Angle);
      return Result;
   end ConstructRotXMatrix3D;

   --  Function to construct a Rotation Matrix3D around the Y-axis
   function ConstructRotYMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D is
      Result : Matrix3D;
   begin
      Result.x1 := LARGE_FLOAT_Functions.Cos (Angle);
      Result.x3 := LARGE_FLOAT_Functions.Sin (Angle);
      Result.y2 := 1.0;
      Result.z1 := -LARGE_FLOAT_Functions.Sin (Angle);
      Result.z3 := LARGE_FLOAT_Functions.Cos (Angle);
      return Result;
   end ConstructRotYMatrix3D;

   --  Function to construct a Rotation Matrix3D around the Z-axis
   function ConstructRotZMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D is
      Result : Matrix3D;
   begin
      Result.x1 := LARGE_FLOAT_Functions.Cos (Angle);
      Result.x2 := -LARGE_FLOAT_Functions.Sin (Angle);
      Result.y1 := LARGE_FLOAT_Functions.Sin (Angle);
      Result.y2 := LARGE_FLOAT_Functions.Cos (Angle);
      Result.z3 := 1.0;
      return Result;
   end ConstructRotZMatrix3D;

   --  Function to construct a Ray3D
   function ConstructRay3D (Org : in Point3D; Dir : in Vector3D) return Ray3D is
      Result : Ray3D;
   begin
      Result.Org := Org;
      Result.Dir := Dir;
      return Result;
   end ConstructRay3D;

   ---------------------------
   --  VECTOR3D OPERATIONS  --
   ---------------------------

   --  Function to return the X-coordinate
   function GetX (Vec : in Vector3D) return LARGE_FLOAT is
   begin
      return Vec.x;
   end GetX;

   --  Function to return the Y-coordinate
   function GetY (Vec : in Vector3D) return LARGE_FLOAT is
   begin
      return Vec.y;
   end GetY;

   --  Function to return the Z-coordinate
   function GetZ (Vec : in Vector3D) return LARGE_FLOAT is
   begin
      return Vec.z;
   end GetZ;

   --  Function to add two Vector3D variables
   function "+" (Left, Right : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x + Right.x;
      Result.y := Left.y + Right.y;
      Result.z := Left.z + Right.z;
      return Result;
   end "+";

   --  Function to substract two Vector3D variables
   function "-" (Left, Right : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x - Right.x;
      Result.y := Left.y - Right.y;
      Result.z := Left.z - Right.z;
      return Result;
   end "-";

   --  Function to negate a Vector3D variable
   function "-" (Vec : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := -Vec.x;
      Result.y := -Vec.y;
      Result.z := -Vec.z;
      return Result;
   end "-";

   --  Function to multiply a Vector3D variable with a LARGE_FLOAT
   function "*" (Left : in Vector3D; Right : LARGE_FLOAT) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x * Right;
      Result.y := Left.y * Right;
      Result.z := Left.z * Right;
      return Result;
   end "*";

   --  Function to multiply a Vector3D variable with a LARGE_FLOAT
   function "*" (Left : in LARGE_FLOAT; Right : Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Right.x * Left;
      Result.y := Right.y * Left;
      Result.z := Right.z * Left;
      return Result;
   end "*";

   --  Function to divide a Vector3D variable with a LARGE_FLOAT
   function "/" (Left : in Vector3D; Right : LARGE_FLOAT) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x / Right;
      Result.y := Left.y / Right;
      Result.z := Left.z / Right;
      return Result;

   exception
      when Constraint_Error =>
         begin
            Put_Line ("*** ERROR in Math3D.""/""(Vector3D): ");
            Put_Line ("          Parameter ""Right"" is zero");
            return Result;
         end;

   end "/";

   --  Function to calculate the Dot Product of two Vector3D variables
   function "*" (Left, Right : in Vector3D) return LARGE_FLOAT is
   begin
      return Left.x * Right.x + Left.y * Right.y + Left.z * Right.z;
   end "*";

   --  Function to calculate the Cross Product of two Vector3D variables
   function "**" (Left, Right : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.y * Right.z - Left.z * Right.y;
      Result.y := Left.z * Right.x - Left.x * Right.z;
      Result.z := Left.x * Right.y - Left.y * Right.x;
      return Result;
   end "**";

   --  Function to calculate the length of a Vector3D variable
   function Length (Vec : in Vector3D) return LARGE_FLOAT is
   begin
      return LARGE_FLOAT_Functions.Sqrt (Vec.x * Vec.x + Vec.y * Vec.y + Vec.z * Vec.z);
   end Length;

   --  Function to calculate the squared length of a Vector3D variable
   function Length_Squared (Vec : in Vector3D) return LARGE_FLOAT is
   begin
      return Vec.x * Vec.x + Vec.y * Vec.y + Vec.z * Vec.z;
   end Length_Squared;

   --  Function to normalize a Vector3D variable
   function Normalize (Vec : in Vector3D) return Vector3D is
      Result : Vector3D;
      Len    : LARGE_FLOAT;
   begin
      Len      := LARGE_FLOAT_Functions.Sqrt (Vec.x * Vec.x + Vec.y * Vec.y + Vec.z * Vec.z);
      Result.x := Vec.x / Len;
      Result.y := Vec.y / Len;
      Result.z := Vec.z / Len;
      return Result;

   exception
      when Constraint_Error =>
         begin
            Put_Line ("*** ERROR in Math3D.Normalize(Vector3D) :");
            Put_Line ("          Length is zero");
            return Result;
         end;

   end Normalize;

   --  Function to transform a Vector3D with a Matrix3D
   function "*" (Mat : in Matrix3D; Vec : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Mat.x1 * Vec.x + Mat.x2 * Vec.y + Mat.x3 * Vec.z;
      Result.y := Mat.y1 * Vec.x + Mat.y2 * Vec.y + Mat.y3 * Vec.z;
      Result.z := Mat.z1 * Vec.x + Mat.z2 * Vec.y + Mat.z3 * Vec.z;
      return Result;
   end "*";

   --------------------------
   --  POINT3D OPERATIONS  --
   --------------------------

   --  Function to return the X-coordinate
   function GetX (Pnt : in Point3D) return LARGE_FLOAT is
   begin
      return Pnt.x;
   end GetX;

   --  Function to return the Y-coordinate
   function GetY (Pnt : in Point3D) return LARGE_FLOAT is
   begin
      return Pnt.y;
   end GetY;

   --  Function to return the Z-coordinate
   function GetZ (Pnt : in Point3D) return LARGE_FLOAT is
   begin
      return Pnt.z;
   end GetZ;

   --  Function to add a Vector3D to a Point3D
   function "+" (Left : in Point3D; Right : in Vector3D) return Point3D is
      Result : Point3D;
   begin
      Result.x := Left.x + Right.x;
      Result.y := Left.y + Right.y;
      Result.z := Left.z + Right.z;
      return Result;
   end "+";

   --  Function to substract a Vector3D from a Point3D
   function "-" (Left : in Point3D; Right : in Vector3D) return Point3D is
      Result : Point3D;
   begin
      Result.x := Left.x - Right.x;
      Result.y := Left.y - Right.y;
      Result.z := Left.z - Right.z;
      return Result;
   end "-";

   --  Function to substract two Point3D variables
   function "-" (Left, Right : in Point3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x - Right.x;
      Result.y := Left.y - Right.y;
      Result.z := Left.z - Right.z;
      return Result;
   end "-";

   --  Function to multiply a Point3D variable with a LARGE_FLOAT
   function "*" (Left : in Point3D; Right : LARGE_FLOAT) return Point3D is
      Result : Point3D;
   begin
      Result.x := Left.x * Right;
      Result.y := Left.y * Right;
      Result.z := Left.z * Right;
      return Result;
   end "*";

   --  Function to multiply a Point3D variable with a LARGE_FLOAT
   function "*" (Left : in LARGE_FLOAT; Right : Point3D) return Point3D is
      Result : Point3D;
   begin
      Result.x := Right.x * Left;
      Result.y := Right.y * Left;
      Result.z := Right.z * Left;
      return Result;
   end "*";

   --  Function to calculate the distance between two Point3D variables
   function Distance (P1, P2 : in Point3D) return LARGE_FLOAT is
      x, y, z : LARGE_FLOAT;
   begin
      x := P1.x - P2.x;
      y := P1.y - P2.y;
      z := P1.z - P2.z;
      return LARGE_FLOAT_Functions.Sqrt (x * x + y * y + z * z);
   end Distance;

   --  Function to calculate the squared distance between two Point3D variables
   function Distance_Squared (P1, P2 : in Point3D) return LARGE_FLOAT is
      x, y, z : LARGE_FLOAT;
   begin
      x := P1.x - P2.x;
      y := P1.y - P2.y;
      z := P1.z - P2.z;
      return x * x + y * y + z * z;
   end Distance_Squared;

   --  Function to calculate the distance between a Point3D and the Origin
   function Distance_Org (P : in Point3D) return LARGE_FLOAT is
   begin
      return LARGE_FLOAT_Functions.Sqrt (P.x * P.x + P.y * P.y + P.z * P.z);
   end Distance_Org;

   --  Function to calculate the squared distance between a Point3D and the Origin
   function Distance_Squared_Org (P : in Point3D) return LARGE_FLOAT is
   begin
      return P.x * P.x + P.y * P.y + P.z * P.z;
   end Distance_Squared_Org;

   --  Function to transform a Point3D with a Matrix3D
   function "*" (Mat : in Matrix3D; Pnt : in Point3D) return Point3D is
      Result : Point3D;
   begin
      Result.x := Mat.x1 * Pnt.x + Mat.x2 * Pnt.y + Mat.x3 * Pnt.z + Mat.tx;
      Result.y := Mat.y1 * Pnt.x + Mat.y2 * Pnt.y + Mat.y3 * Pnt.z + Mat.ty;
      Result.z := Mat.z1 * Pnt.x + Mat.z2 * Pnt.y + Mat.z3 * Pnt.z + Mat.tz;
      return Result;
   end "*";

   ---------------------------
   --  NORMAL3D OPERATIONS  --
   ---------------------------

   --  Function to add two Normal3D variables
   function "+" (Left, Right : in Normal3D) return Normal3D is
      Result : Normal3D;
   begin
      Result.x := Left.x + Right.x;
      Result.y := Left.y + Right.y;
      Result.z := Left.z + Right.z;
      return Result;
   end "+";

   --  Function to negate a Normal3D variable
   function "-" (Norm : in Normal3D) return Normal3D is
      Result : Normal3D;
   begin
      Result.x := -Norm.x;
      Result.y := -Norm.y;
      Result.z := -Norm.z;
      return Result;
   end "-";

   --  Function to multiply a Normal3D variable with a LARGE_FLOAT
   function "*" (Left : in Normal3D; Right : in LARGE_FLOAT) return Normal3D is
      Result : Normal3D;
   begin
      Result.x := Left.x * Right;
      Result.y := Left.y * Right;
      Result.z := Left.z * Right;
      return Result;
   end "*";

   --  Function to multiply a Normal3D variable with a LARGE_FLOAT
   function "*" (Left : in LARGE_FLOAT; Right : in Normal3D) return Normal3D is
      Result : Normal3D;
   begin
      Result.x := Right.x * Left;
      Result.y := Right.y * Left;
      Result.z := Right.z * Left;
      return Result;
   end "*";

   --  Function to add a Normal3D variable to a Vector3D
   function "+" (Left : in Normal3D; Right : in Vector3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x + Right.x;
      Result.y := Left.y + Right.y;
      Result.z := Left.z + Right.z;
      return Result;
   end "+";

   --  Function to add a Vector3D variable to a Normal3D
   function "+" (Left : in Vector3D; Right : in Normal3D) return Vector3D is
      Result : Vector3D;
   begin
      Result.x := Left.x + Right.x;
      Result.y := Left.y + Right.y;
      Result.z := Left.z + Right.z;
      return Result;
   end "+";

   --  Function to calculate the dot product of a Normal3D and a Vector3D
   function "*" (Left : in Normal3D; Right : in Vector3D) return LARGE_FLOAT is
   begin
      return Left.x * Right.x + Left.y * Right.y + Left.z * Right.z;
   end "*";

   --  Function to calculate the dot product of a Vector3D and a Normal3D
   function "*" (Left : in Vector3D; Right : in Normal3D) return LARGE_FLOAT is
   begin
      return Left.x * Right.x + Left.y * Right.y + Left.z * Right.z;
   end "*";

   --  Function to normalize a Normal3D variable
   function Normalize (Norm : in Normal3D) return Normal3D is
      Result : Normal3D;
      Len    : LARGE_FLOAT;
   begin
      Len      :=
         LARGE_FLOAT_Functions.Sqrt (Norm.x * Norm.x + Norm.y * Norm.y + Norm.z * Norm.z);
      Result.x := Norm.x / Len;
      Result.y := Norm.y / Len;
      Result.z := Norm.z / Len;
      return Result;

   exception
      when Constraint_Error =>
         begin
            Put_Line ("*** ERROR in Math3D.Normalize(Normal3D) :");
            Put_Line ("          Length is zero");
            return Result;
         end;

   end Normalize;

   --  Function to transform a Normal3D with a Matrix3D
   function "*" (Mat : in Matrix3D; Norm : in Normal3D) return Normal3D is
      Result : Normal3D;
   begin
      Result.x := Mat.x1 * Norm.x + Mat.y1 * Norm.y + Mat.z1 * Norm.z;
      Result.x := Mat.x2 * Norm.x + Mat.y2 * Norm.y + Mat.z2 * Norm.z;
      Result.x := Mat.x3 * Norm.x + Mat.y3 * Norm.y + Mat.z3 * Norm.z;
      return Result;
   end "*";

   ---------------------------
   --  MATRIX3D OPERATIONS  --
   ---------------------------

   --  Function to multiply two Matrix3D variables
   function "*" (Left, Right : in Matrix3D) return Matrix3D is
      Result : Matrix3D;
   begin
      Result.x1 := Left.x1 * Right.x1 + Left.x2 * Right.y1 + Left.x3 * Right.z1;
      Result.x2 := Left.x1 * Right.x2 + Left.x2 * Right.y2 + Left.x3 * Right.z2;
      Result.x3 := Left.x1 * Right.x3 + Left.x2 * Right.y3 + Left.x3 * Right.z3;
      Result.tx := Left.x1 * Right.tx + Left.x2 * Right.ty + Left.x3 * Right.tz + Left.tz;

      Result.y1 := Left.y1 * Right.x1 + Left.y2 * Right.y1 + Left.y3 * Right.z1;
      Result.y2 := Left.y1 * Right.x2 + Left.y2 * Right.y2 + Left.y3 * Right.z2;
      Result.y3 := Left.y1 * Right.x3 + Left.y2 * Right.y3 + Left.y3 * Right.z3;
      Result.ty := Left.y1 * Right.tx + Left.y2 * Right.ty + Left.y3 * Right.tz + Left.ty;

      Result.z1 := Left.z1 * Right.x1 + Left.z2 * Right.y1 + Left.z3 * Right.z1;
      Result.z2 := Left.z1 * Right.x2 + Left.z2 * Right.y2 + Left.z3 * Right.z2;
      Result.z3 := Left.z1 * Right.x3 + Left.z2 * Right.y3 + Left.z3 * Right.z3;
      Result.tz := Left.z1 * Right.tx + Left.z2 * Right.ty + Left.z3 * Right.tz + Left.tz;

      return Result;
   end "*";

   --  Function to calculate the inverse of a Matrix3D
   function Inverse (Mat : in Matrix3D) return Matrix3D is
      Result : Matrix3D;
      Det    : LARGE_FLOAT;
   begin
      Det := Mat.x1 * Mat.y2 * Mat.z3 +
             Mat.x2 * Mat.y3 * Mat.z1 +
             Mat.x3 * Mat.y1 * Mat.z2 -
             Mat.x1 * Mat.y3 * Mat.z2 -
             Mat.x2 * Mat.y1 * Mat.z3 -
             Mat.x3 * Mat.y2 * Mat.z1;

      Result.x1 := (Mat.y2 * Mat.z3 - Mat.y3 * Mat.z2) / Det;
      Result.x2 := (Mat.x3 * Mat.z2 - Mat.x2 * Mat.z3) / Det;
      Result.x3 := (Mat.x2 * Mat.y3 - Mat.x3 * Mat.y2) / Det;
      Result.y1 := (Mat.y3 * Mat.z1 - Mat.y1 * Mat.z3) / Det;
      Result.y2 := (Mat.x1 * Mat.z3 - Mat.x3 * Mat.z1) / Det;
      Result.y3 := (Mat.x3 * Mat.y1 - Mat.x1 * Mat.y3) / Det;
      Result.z1 := (Mat.y1 * Mat.z2 - Mat.y2 * Mat.z1) / Det;
      Result.z2 := (Mat.x2 * Mat.z1 - Mat.x1 * Mat.z2) / Det;
      Result.z3 := (Mat.x1 * Mat.y2 - Mat.x2 * Mat.y1) / Det;
      Result.tx :=
        (Mat.x2 * Mat.ty * Mat.z3 +
         Mat.x3 * Mat.y2 * Mat.tz +
         Mat.tx * Mat.y3 * Mat.z2 -
         Mat.x2 * Mat.y3 * Mat.tz -
         Mat.x3 * Mat.ty * Mat.z2 -
         Mat.tx * Mat.y2 * Mat.z3) /
        Det;
      Result.ty :=
        (Mat.x1 * Mat.y3 * Mat.tz +
         Mat.x3 * Mat.ty * Mat.z1 +
         Mat.tx * Mat.y1 * Mat.z3 -
         Mat.x1 * Mat.ty * Mat.z3 -
         Mat.x3 * Mat.y1 * Mat.tz -
         Mat.tx * Mat.y3 * Mat.z1) /
        Det;
      Result.tz :=
        (Mat.x1 * Mat.ty * Mat.z2 +
         Mat.x2 * Mat.y1 * Mat.tz +
         Mat.tx * Mat.y2 * Mat.z1 -
         Mat.x1 * Mat.y2 * Mat.tz -
         Mat.x2 * Mat.ty * Mat.z1 -
         Mat.tx * Mat.y1 * Mat.z2) /
        Det;
      return Result;

   exception
      when Constraint_Error =>
         begin
            Put_Line ("*** ERROR in Math3D.Inverse: ");
            Put_Line ("          Determinant is zero");
            return Result;
         end;

   end Inverse;

   ------------------------
   --  RAY3D OPERATIONS  --
   ------------------------

   --  Function to return the origin of a Ray3D as a Point3D
   function GetRay3DOrigin (Ray : in Ray3D) return Point3D is
   begin
      return Ray.Org;
   end GetRay3DOrigin;

   --  Function to return the direction of a Ray3D as a Point3D
   function GetRay3DDirection (Ray : in Ray3D) return Vector3D is
   begin
      return Ray.Dir;
   end GetRay3DDirection;

   --  Function to return a point on the ray, as in Org + Lambda*Dir
   function GetPoint3DOnRay3D (Lambda : in LARGE_FLOAT; Ray : in Ray3D) return Point3D is
   begin
      return Ray.Org + Lambda * Ray.Dir;
   end GetPoint3DOnRay3D;

   --  Function to return a point on a line, as in Org + Lambda*Dir
   function GetPoint3DOnOrgDir
     (Lambda : in LARGE_FLOAT;
      Org    : in Point3D;
      Dir    : Vector3D)
      return   Point3D
   is
   begin
      return Org + Lambda * Dir;
   end GetPoint3DOnOrgDir;

end Math3D;

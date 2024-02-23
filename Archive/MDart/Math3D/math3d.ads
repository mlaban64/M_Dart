--  <description>
--  Package Math3D contains the basic types and functions to handle the linear algebra
--  that is required for computer graphics in general and ray tracing specifically
--
--  It intentionally avoids using any standard packages in Ada, so maximum flexibility
--  and efficiency is reached
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes; use StdTypes;

package Math3D is

   ------------------------
   --  TYPE DEFINITIONS  --
   ------------------------

   type Point2D is private;
   --  <summary>Point2D type stores a 2D Point</summary>

   type Point3D is private;
   --  <summary>Point3D type stores a 3D Point</summary>

   type Vector3D is private;
   --  <summary>Vector3D type stores a 3D Vector</summary>

   type Normal3D is private;
   --  <summary>Normal3D type stores a 3D Normal</summary>

   type Matrix3D is private;
   --  <summary>Matrix3D type stores a 3D Matrix</summary>

   type Ray3D is private;
   --  <summary>Ray3D type is used to represent a Ray in 3D</summary>

   ---------------------------------------
   --  SUPPORTING TEXT I/O SUBPROGRAMS  --
   ---------------------------------------

   procedure Put (Point : in Point2D);
   --  <summary>Put procedure to print a 2D Point</summary>
   --  <parameter name="Point">A Point2D</parameter>
   --  <exception>No exception</exception>

   procedure Put (Point : in Point3D);
   --  <summary>Put procedure to print a 3D Point</summary>
   --  <parameter name="Point">A Point3D</parameter>
   --  <exception>No exception</exception>

   procedure Put (Vector : in Vector3D);
   --  <summary>Put procedure to print a 3D Vector</summary>
   --  <parameter name="Vector">A Vector3D</parameter>
   --  <exception>No exception</exception>

   procedure Put (Normal : in Normal3D);
   --  <summary>Put procedure to print a 3D Normal</summary>
   --  <parameter name="Normal">A Normal3D</parameter>
   --  <exception>No exception</exception>

   procedure Put (Matrix : in Matrix3D);
   --  <summary>Put procedure to print a 3D Matrix</summary>
   --  <parameter name="Matrix">A Matrix3D</parameter>
   --  <exception>No exception</exception>

   procedure Put (Ray : in Ray3D);
   --  <summary>Put procedure to print an Ray3D</summary>
   --  <parameter name="Ray">A Ray3D</parameter>
   --  <exception>No exception</exception>

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructPoint2D (x, y : in LARGE_FLOAT) return Point2D;
   --  <summary>function to construct a Point2D from two Floats</summary>
   --  <parameter name="x">A LARGE_FLOAT</parameter>
   --  <parameter name="y">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructPoint3D (x, y, z : in LARGE_FLOAT) return Point3D;
   --  <summary>function to construct a Point3D from three Floats</summary>
   --  <parameter name="x">A LARGE_FLOAT</parameter>
   --  <parameter name="y">A LARGE_FLOAT</parameter>
   --  <parameter name="z">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructVector3D (x, y, z : in LARGE_FLOAT) return Vector3D;
   --  <summary>function to construct a Vector3D from three Floats</summary>
   --  <parameter name="x">A LARGE_FLOAT</parameter>
   --  <parameter name="y">A LARGE_FLOAT</parameter>
   --  <parameter name="z">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructNormal3D (x, y, z : in LARGE_FLOAT) return Normal3D;
   --  <summary>function to construct a Normal3D from three Floats</summary>
   --  <parameter name="x">A LARGE_FLOAT</parameter>
   --  <parameter name="y">A LARGE_FLOAT</parameter>
   --  <parameter name="z">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructUnitMatrix3D return Matrix3D;
   --  <summary>function to construct a Unit Matrix3D</summary>
   --  <parameter>No parameters</parameter>
   --  <exception>No exception</exception>

   function ConstructTranslateMatrix3D (tx, ty, tz : in LARGE_FLOAT) return Matrix3D;
   --  <summary>function to construct a Translation Matrix3D, using
   --  tx, ty and tz as the translation direction</summary>
   --  <parameter name="tx">A LARGE_FLOAT</parameter>
   --  <parameter name="ty">A LARGE_FLOAT</parameter>
   --  <parameter name="tz">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructScaleMatrix3D (sx, sy, sz : in LARGE_FLOAT) return Matrix3D;
   --  <summary>function to construct a Scaling Matrix3D, using
   --  sx, sy and sz as the scaling factors</summary>
   --  <parameter name="sx">A LARGE_FLOAT</parameter>
   --  <parameter name="sy">A LARGE_FLOAT</parameter>
   --  <parameter name="sz">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructShearMatrix3D
     (Hxy, Hxz, Hyx, Hyz, Hzx, Hzy : in LARGE_FLOAT)
      return                         Matrix3D;
   --  <summary>function to construct a Shearing Matrix3D, using
   --  Hxy, Hxz, Hyx, Hyz, Hzx and Hzy as the shearing factors</summary>
   --  <parameter name="Hxy">A LARGE_FLOAT</parameter>
   --  <parameter name="Hxz">A LARGE_FLOAT</parameter>
   --  <parameter name="Hyx">A LARGE_FLOAT</parameter>
   --  <parameter name="Hyz">A LARGE_FLOAT</parameter>
   --  <parameter name="Hzx">A LARGE_FLOAT</parameter>
   --  <parameter name="Hzy">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructRotXMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D;
   --  <summary>function to construct a Rotation Matrix3D, using
   --  the Angle parameter as the rotation angle around the X-axis</summary>
   --  <parameter name="Angle">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructRotYMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D;
   --  <summary>function to construct a Rotation Matrix3D, using
   --  the Angle parameter as the rotation angle around the Y-axis</summary>
   --  <parameter name="Angle">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructRotZMatrix3D (Angle : in LARGE_FLOAT) return Matrix3D;
   --  <summary>function to construct a Rotation Matrix3D, using
   --  the Angle parameter as the rotation angle around the Z-axis</summary>
   --  <parameter name="Angle">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function ConstructRay3D (Org : in Point3D; Dir : in Vector3D) return Ray3D;
   --  <summary>function to construct a Ray3D by passing in the Origin and Direction</summary>
   --  <parameter name="Org">A Point3D</parameter>
   --  <parameter name="Dir">A vector3D</parameter>
   --  <exception>No exception</exception>

   ---------------------------
   --  VECTOR3D OPERATIONS  --
   ---------------------------

   function GetX (Vec : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to return the X-coordinate of a Vector3D</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function GetY (Vec : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to return the Y-coordinate of a Vector3D</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function GetZ (Vec : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to return the Z-coordinate of a Vector3D</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "+" (Left, Right : in Vector3D) return Vector3D;
   --  <summary>function to add two Vector3D variables.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "-" (Left, Right : in Vector3D) return Vector3D;
   --  <summary>function to substract two Vector3D variables.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "-" (Vec : in Vector3D) return Vector3D;
   --  <summary>function to negate a Vector3D variable.
   --  It returns a Vector3D</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in Vector3D; Right : LARGE_FLOAT) return Vector3D;
   --  <summary>function to multiply a Vector3D variable with a LARGE_FLOAT.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in LARGE_FLOAT; Right : Vector3D) return Vector3D;
   --  <summary>function to multiply a Vector3D variable with a LARGE_FLOAT.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A LARGE_FLOAT</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "/" (Left : in Vector3D; Right : LARGE_FLOAT) return Vector3D;
   --  <summary>function to divide a Vector3D variable with a LARGE_FLOAT.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function "*" (Left, Right : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to calculate the dot product of two Vector3D variables.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "**" (Left, Right : in Vector3D) return Vector3D;
   --  <summary>function to calculate the cross product of two Vector3D variables.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function Length (Vec : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to calculate the length of a Vector3D variable.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function Length_Squared (Vec : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to calculate the squared length of a Vector3D variable.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function Normalize (Vec : in Vector3D) return Vector3D;
   --  <summary>function to normalize a Vector3D variable.
   --  It returns a Vector3D</summary>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Mat : in Matrix3D; Vec : in Vector3D) return Vector3D;
   --  <summary>function to transform a Vector3D with a Matrix3D. It is assumed that the Matrix3D
   --  has (0 0 0 1) as the bottom row for values. It returns a Vector3D</summary>
   --  <parameter name="Mat">A Matrix3D</parameter>
   --  <parameter name="Vec">A Vector3D</parameter>
   --  <exception>No exception</exception>

   --------------------------
   --  POINT3D OPERATIONS  --
   --------------------------

   function GetX (Pnt : in Point3D) return LARGE_FLOAT;
   --  <summary>function to return the X-coordinate of a Point3D</summary>
   --  <parameter name="Pnt">A Point3D</parameter>
   --  <exception>No exception</exception>

   function GetY (Pnt : in Point3D) return LARGE_FLOAT;
   --  <summary>function to return the Y-coordinate of a Point3D</summary>
   --  <parameter name="Pnt">A Point3D</parameter>
   --  <exception>No exception</exception>

   function GetZ (Pnt : in Point3D) return LARGE_FLOAT;
   --  <summary>function to return the Z-coordinate of a Point3D</summary>
   --  <parameter name="Pnt">A Point3D</parameter>
   --  <exception>No exception</exception>

   function "+" (Left : in Point3D; Right : in Vector3D) return Point3D;
   --  <summary>function to add a Vector3D to a Point3D.
   --  It implies a transform of a Point3D and returns a Point3D</summary>
   --  <parameter name="Left">A Point3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "-" (Left : in Point3D; Right : in Vector3D) return Point3D;
   --  <summary>function to substract a Vector3D from a Point3D.
   --  It implies a transform of a Point3D and returns a Point3D</summary>
   --  <parameter name="Left">A Point3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "-" (Left, Right : in Point3D) return Vector3D;
   --  <summary>function to substract two Point3D variables.
   --  It calculates the vector from Right to Left. It returns a Vector3D</summary>
   --  <parameter name="Left">A Point3D</parameter>
   --  <parameter name="Right">A Point3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in Point3D; Right : LARGE_FLOAT) return Point3D;
   --  <summary>function to multiply a Point3D variable with a LARGE_FLOAT.
   --  It returns a Point3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in LARGE_FLOAT; Right : Point3D) return Point3D;
   --  <summary>function to multiply a Point3D variable with a LARGE_FLOAT.
   --  It returns a Point3D</summary>
   --  <parameter name="Left">A LARGE_FLOAT</parameter>
   --  <parameter name="Right">A Point3D</parameter>
   --  <exception>No exception</exception>

   function Distance (P1, P2 : in Point3D) return LARGE_FLOAT;
   --  <summary>function to calculate the distance between two Point3D variables,
   --  as in ||P1 - P2||. It returns a LARGE_FLOAT</summary>
   --  <parameter name="P1">A Point3D</parameter>
   --  <parameter name="P2">A Point3D</parameter>
   --  <exception>No exception</exception>

   function Distance_Squared (P1, P2 : in Point3D) return LARGE_FLOAT;
   --  <summary>function to calculate the squared distance between two Point3D variables,
   --  as in ||P1 - P2||^2.It returns a LARGE_FLOAT</summary>
   --  <parameter name="P1">A Point3D</parameter>
   --  <parameter name="P2">A Point3D</parameter>
   --  <exception>No exception</exception>

   function Distance_Org (P: in Point3D) return LARGE_FLOAT;
   --  <summary>function to calculate the distance between a Point3D and the Origin.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="P">A Point3D</parameter>
   --  <exception>No exception</exception>

   function Distance_Squared_Org (P: in Point3D) return LARGE_FLOAT;
   --  <summary>function to calculate the squared distance between a Point3D and the Origin.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="P">A Point3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Mat : in Matrix3D; Pnt : in Point3D) return Point3D;
   --  <summary>function to transform a Point3D with a Matrix3D. It is assumed that the Matrix3D
   --  has (0 0 0 1) as the bottom row for values. It returns a Point3D</summary>
   --  <parameter name="Mat">A Matrix3D</parameter>
   --  <parameter name="Pnt">A Point3D</parameter>
   --  <exception>No exception</exception>

   ---------------------------
   --  NORMAL3D OPERATIONS  --
   ---------------------------

   function "+" (Left, Right : in Normal3D) return Normal3D;
   --  <summary>function to add two Normal3D variables.
   --  It returns a Normal3D</summary>
   --  <parameter name="Left">A Normal3D</parameter>
   --  <parameter name="Right">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function "-" (Norm : in Normal3D) return Normal3D;
   --  <summary>function to negate a Normal3D variable.
   --  It returns a Vector3D</summary>
   --  <parameter name="Norm">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in Normal3D; Right : in LARGE_FLOAT) return Normal3D;
   --  <summary>function to multiply a Normal3D variable with a LARGE_FLOAT.
   --  It returns a Normal3D</summary>
   --  <parameter name="Left">A Normal3D</parameter>
   --  <parameter name="Right">A LARGE_FLOAT</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in LARGE_FLOAT; Right : in Normal3D) return Normal3D;
   --  <summary>function to multiply a Normal3D variable with a LARGE_FLOAT.
   --  It returns a Normal3D</summary>
   --  <parameter name="Left">A LARGE_FLOAT</parameter>
   --  <parameter name="Right">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function "+" (Left : in Normal3D; Right : in Vector3D) return Vector3D;
   --  <summary>function to add a Normal3D variable to a Vector3D.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Normal3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "+" (Left : in Vector3D; Right : in Normal3D) return Vector3D;
   --  <summary>function to add a Vector3D variable to a Normal3D.
   --  It returns a Vector3D</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in Normal3D; Right : in Vector3D) return LARGE_FLOAT;
   --  <summary>function to calculate the dot product of a Normal3D and a Vector3D.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="Left">A Normal3D</parameter>
   --  <parameter name="Right">A Vector3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Left : in Vector3D; Right : in Normal3D) return LARGE_FLOAT;
   --  <summary>function to calculate the dot product of a Vector3D and a Normal3D.
   --  It returns a LARGE_FLOAT</summary>
   --  <parameter name="Left">A Vector3D</parameter>
   --  <parameter name="Right">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function Normalize (Norm : in Normal3D) return Normal3D;
   --  <summary>function to normalize a Normal3D variable.
   --  It returns a Normal3D</summary>
   --  <parameter name="Norm">A Normal3D</parameter>
   --  <exception>No exception</exception>

   function "*" (Mat : in Matrix3D; Norm : in Normal3D) return Normal3D;
   --  <summary>function to transform a Normal3D with a Matrix3D. It is assumed that the Matrix3D
   --  has (0 0 0 1) as the bottom row for values. The Matrix3D is transposed in the calculation
   --  for the result. It returns a Normal3D</summary>
   --  <parameter name="Mat">A Matrix3D</parameter>
   --  <parameter name="Norm">A Normal3D</parameter>
   --  <exception>No exception</exception>

   ---------------------------
   --  MATRIX3D OPERATIONS  --
   ---------------------------

   function "*" (Left, Right : in Matrix3D) return Matrix3D;
   --  <summary>function to multiply two Matrix3D variables. It is assumed that both the Matrix3D
   --  have (0 0 0 1) as the bottom row for values. It returns a Matrix3D</summary>
   --  <parameter name="Left">A Matrix3D</parameter>
   --  <parameter name="Right">A Matrix3D</parameter>
   --  <exception>No exception</exception>

   function Inverse (Mat : in Matrix3D) return Matrix3D;
   --  <summary>function to calculate the inverse of a Matrix3D. It is assumed that the Matrix3D
   --  has (0 0 0 1) as the bottom row for values. It returns a Matrix3D</summary>
   --  <parameter name="Mat">A Matrix3D</parameter>
   --  <exception>No exception</exception>

   ------------------------
   --  RAY3D OPERATIONS  --
   ------------------------

   function GetRay3DOrigin (Ray : in Ray3D) return Point3D;
   --  <summary>function to return the origin of a Ray3D as a Point3D</summary>
   --  <parameter name="Ray">A Ray3D</parameter>
   --  <exception>No exception</exception>

   function GetRay3DDirection (Ray : in Ray3D) return Vector3D;
   --  <summary>function to return the direction of a Ray3D as a Vector3D</summary>
   --  <parameter name="Ray">A Ray3D</parameter>
   --  <exception>No exception</exception>

   function GetPoint3DOnRay3D (Lambda : in LARGE_FLOAT; Ray : in Ray3D) return Point3D;
   --  <summary>function to return a point on the ray, as in Org + Lambda*Dir</summary>
   --  <parameter name="Lambda">A distance along the ray</parameter>
   --  <parameter name="Ray">A Ray3D</parameter>
   --  <exception>No exception</exception>

   function GetPoint3DOnOrgDir (Lambda : in LARGE_FLOAT; Org : in Point3D; Dir : Vector3D) return Point3D;
   --  <summary>function to return a point on a line, as in Org + Lambda*Dir</summary>
   --  <parameter name="Lambda">A distance along the ray</parameter>
   --  <parameter name="Org">A Point3D as Origin of the line</parameter>
   --  <parameter name="Org">A Vector3D3D as Direction of the line</parameter>
   --  <exception>No exception</exception>

private
   type Point2D is record
      x, y : LARGE_FLOAT := 0.0;
   end record;

   type Point3D is record
      x, y, z : LARGE_FLOAT := 0.0;
   end record;

   type Vector3D is record
      x, y : LARGE_FLOAT := 0.0;
      z    : LARGE_FLOAT := 1.0;
   end record;

   type Normal3D is record
      x, y : LARGE_FLOAT := 0.0;
      z    : LARGE_FLOAT := 1.0;
   end record;

   --  Matrix3D assumes the following structure
   --
   --  (x1 x2 x3 tx)
   --  (y1 y2 y3 ty)
   --  (z1 z2 z3 tz)
   --  (0  0  0  1 )
   --
   --  Since the last row is always (0 0 0 1), it is not stored...
   --  This does imply that storage of a transposed matrix is not possible!
   --  By default, a Matrix3D will be the Unit Matrix
   --
   type Matrix3D is record
      x2, x3, tx : LARGE_FLOAT := 0.0;
      y1, y3, ty : LARGE_FLOAT := 0.0;
      z1, z2, tz : LARGE_FLOAT := 0.0;
      x1, y2, z3 : LARGE_FLOAT := 1.0;
   end record;

   type Ray3D is record
      Org : Point3D;
      --  Org is the ray origin;
      Dir : Vector3D;
      --  Dir is the ray direction;
   end record;

end Math3D;

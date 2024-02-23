with Core_Types; use Core_Types;

--<summary>The LinearMath package contains all Abstract Data Types for 2D & 3D Vector operations</summary>
--<description>The LinearMath package contains all Abstract Data Types for 2D & 3D Vector operations</description>
--<group>Core</group>
package Linear_Math is

   -------------------------------------------------------------------------------
   --  The Main Abstract Data Types are declared here to avoid forward referencing
   -------------------------------------------------------------------------------

   type Point_2D is private;
   --<summary>ADT for a point in 2D</summary>

   type Point_3D is private;
   --<summary>ADT for a point in 3D</summary>

   type Vector_3D is private;
   --<summary>ADT for a vector in 3D</summary>

   type Normal_3D is private;
   --<summary>ADT for a normal vector in 3D</summary>

   type Matrix_3D is private;
   --<summary>ADT for a matrix in 3D</summary>
   --<description>Since the last row is always (0 0 0 1), it is not stored. By default, a Matrix_3D is the unity matrix.
   --For a Matrix_3D, the following structure is implemented.</description>
   --<code>
   --  (X1 X2 X3 Tx)
   --  (Y1 Y2 Y3 Ty)
   --  (Z1 Z2 Z3 Tz)
   --  ( 0  0  0  1)
   --</code>

   type Ray is private;
   --<summary>ADT for a ray in 3D</summary>

   ------------------------------------------------------
   --  THE 2D TYPES ARE BASED UPON AN {X, Y, 1} STRUCTURE
   ------------------------------------------------------
   ----------------
   --  Point_2D ADT
   ----------------

   function Construct_Point (X, Y : in Large_Float) return Point_2D;
   --<summary>Constructs a Point_2D</summary>
   --<description>Constructs a Point_2D with the coordinates passed in. The origin is the default coordinate</description>
   --<parameter name="X">The X-coordinate of the point</parameter>
   --<parameter name="Y">The Y-coordinate of the point</parameter>
   --<exception>None at this moment</exception>

   procedure Put (P : in Point_2D; Msg : in String := "Point_2D = ");
   --<summary>Prints a Point_2D</summary>
   --<description>Prints a Point_2D to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="P">The point to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_X (P : in Point_2D) return Large_Float;
   --<summary>Returns the X-coordinate of a Point_2D</summary>
   --<description>Returns the X-coordinate of a Point_2D</description>
   --<parameter name="P">The point to return the X-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Y (P : in Point_2D) return Large_Float;
   --<summary>Returns the Y-coordinate of a Point_2D</summary>
   --<description>Returns the Y-coordinate of a Point_2D</description>
   --<parameter name="P">The point to return the Y-coordinate from</parameter>
   --<exception>None at this moment</exception>

   ---------------------------------------------------------
   --  THE 3D TYPES ARE BASED UPON AN {X, Y, Z, 1} STRUCTURE
   ---------------------------------------------------------

   ----------------
   --  Point_3D ADT
   ----------------

   function Construct_Point (X, Y, Z : in Large_Float) return Point_3D;
   --<summary>Constructs a Point_3D</summary>
   --<description>Constructs a Point_3D with the coordinates passed in. The origin is the default coordinate</description>
   --<parameter name="X">The X-coordinate of the point</parameter>
   --<parameter name="Y">The Y-coordinate of the point</parameter>
   --<parameter name="Z">The Y-coordinate of the point</parameter>
   --<exception>None at this moment</exception>

   procedure Put (P : in Point_3D; Msg : in String := "Point_3D = ");
   --<summary>Prints a Point_3D</summary>
   --<description>Prints a Point_3D to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="P">The point to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_X (P : in Point_3D) return Large_Float;
   --<summary>Returns the X-coordinate of a Point_3D</summary>
   --<description>Returns the X-coordinate of a Point_3D</description>
   --<parameter name="P">The point to return the X-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Y (P : in Point_3D) return Large_Float;
   --<summary>Returns the Y-coordinate of a Point_3D</summary>
   --<description>Returns the Y-coordinate of a Point_3D</description>
   --<parameter name="P">The point to return the Y-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Z (P : in Point_3D) return Large_Float;
   --<summary>Returns the Z-coordinate of a Point_3D</summary>
   --<description>Returns the Z-coordinate of a Point_3D</description>
   --<parameter name="P">The point to return the Z-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function "+" (P : in Point_3D; V : in Vector_3D) return Point_3D;
   --<summary>Displaces a Point_3D by a Vector_3D</summary>
   --<description>function to displace a Point_3D by adding a Vector_3D</description>
   --<parameter name="P">The point to displace</parameter>
   --<parameter name="V">The vector to displace the point with</parameter>
   --<exception>None at this moment</exception>

   function "-" (P : in Point_3D; V : in Vector_3D) return Point_3D;
   --<summary>Displaces a Point_3D by a Vector_3D</summary>
   --<description>function to displace a Point_3D by subtracting a Vector_3D</description>
   --<parameter name="P">The point to displace</parameter>
   --<parameter name="V">The vector to displace the point with</parameter>
   --<exception>None at this moment</exception>

   function "*" (M : in Matrix_3D; P : in Point_3D) return Point_3D;
   --<summary>Transforms a Point_3D with a Matrix_3D</summary>
   --<description>function to compute a transformed point with a matrix as in P' := M * P</description>
   --<parameter name="M">The matrix</parameter>
   --<parameter name="P">The point</parameter>
   --<exception>None at this moment</exception>

   function Distance (P1, P2 : in Point_3D) return Large_Float;
   --<summary>Returns the distance between two Point_3D variables</summary>
   --<description>function to compute the distance between two Point_3D variables</description>
   --<parameter name="P1">The first point</parameter>
   --<parameter name="P2">The second point</parameter>
   --<exception>None at this moment</exception>

   function Distance_Squared (P1, P2 : in Point_3D) return Large_Float;
   --<summary>Returns the squared distance between two Point_3D variables</summary>
   --<description>function to compute the squared distance between two Point_3D variables</description>
   --<parameter name="P1">The first point</parameter>
   --<parameter name="P2">The second point</parameter>
   --<exception>None at this moment</exception>

   function Distance_To_Org (P : in Point_3D) return Large_Float;
   --<summary>Returns the distance between a Point_3D and the Origin</summary>
   --<description>function to compute the distance between a Point_3D and the Origin</description>
   --<parameter name="P">The point</parameter>
   --<exception>None at this moment</exception>

   function Distance_To_Org_Squared (P : in Point_3D) return Large_Float;
   --<summary>Returns the squared distance between a Point_3D and the Origin</summary>
   --<description>function to compute the squared distance between a Point_3D and the Origin</description>
   --<parameter name="P">The point</parameter>
   --<exception>None at this moment</exception>

   function Point_On_Ray (R : in Ray; Lambda : in Large_Float) return Point_3D;
   --<summary>Returns a point on a ray, as P := Org + Lambda * Dir</summary>
   --<description>function to compute a point on a ray, as P := Org + Lambda * Dir</description>
   --<parameter name="R">The ray to calculate the point on</parameter>
   --<parameter name="Lambda">The parameter to compute the point from</parameter>
   --<exception>None at this moment</exception>

   function Point_On_Ray (Org : in Point_3D; Dir : in Vector_3D; Lambda : in Large_Float) return Point_3D;
   --<summary>Returns a point on a ray, as P := Org + Lambda * Dir</summary>
   --<description>function to compute a point on a ray, as P := Org + Lambda * Dir</description>
   --<parameter name="Org">The origin of the ray to calculate the point on</parameter>
   --<parameter name="Dir">The direction of the ray to calculate the point on</parameter>
   --<parameter name="Lambda">The parameter to compute the point from</parameter>
   --<exception>None at this moment</exception>

   function To_Point_3D (V : in Vector_3D) return Point_3D;
   --<summary>Converts a Vector_3D into a Point_3D</summary>
   --<description>function to convert a Vector_3D into a Point_3D</description>
   --<parameter name="V">The vector to convert</parameter>
   --<exception>None at this moment</exception>

   -----------------
   --  Vector_3D ADT
   -----------------

   function Construct_Vector (X, Y, Z : in Large_Float) return Vector_3D;
   --<summary>Constructs a Vector_3D</summary>
   --<description>Constructs a Vector_3D with the coordinates passed in. The origin is the default coordinate</description>
   --<parameter name="X">The X-coordinate of the point</parameter>
   --<parameter name="Y">The Y-coordinate of the point</parameter>
   --<parameter name="Z">The Y-coordinate of the point</parameter>
   --<exception>None at this moment</exception>

   procedure Put (V : in Vector_3D; Msg : in String := "Vector_3D = ");
   --<summary>Prints a Vector_3D</summary>
   --<description>Prints a Vector_3D to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="V">The vector to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_X (V : in Vector_3D) return Large_Float;
   --<summary>Returns the X-coordinate of a Vector_3D</summary>
   --<description>Returns the X-coordinate of a Vector_3D</description>
   --<parameter name="V">The point to return the X-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Y (V : in Vector_3D) return Large_Float;
   --<summary>Returns the Y-coordinate of a Vector_3D</summary>
   --<description>Returns the Y-coordinate of a Vector_3D</description>
   --<parameter name="V">The point to return the Y-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Z (V : in Vector_3D) return Large_Float;
   --<summary>Returns the Z-coordinate of a Vector_3D</summary>
   --<description>Returns the Z-coordinate of a Vector_3D</description>
   --<parameter name="V">The point to return the Z-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function "-" (V : in Vector_3D) return Vector_3D;
   --<summary>Negates a Vector_3D</summary>
   --<description>function to compute the negative (inverse) version of a Vector_3D</description>
   --<parameter name="V">The vector to negate</parameter>
   --<exception>None at this moment</exception>

   function "+" (V1 : in Vector_3D; V2 : in Vector_3D) return Vector_3D;
   --<summary>Adds two Vector_3D variables</summary>
   --<description>function to add two Vector_3D variables</description>
   --<parameter name="V1">The first vector</parameter>
   --<parameter name="V2">The second vector</parameter>
   --<exception>None at this moment</exception>

   function "-" (V1 : in Vector_3D; V2 : in Vector_3D) return Vector_3D;
   --<summary>Subtracts two Vector_3D variables</summary>
   --<description>function to subtract two Vector_3D variables</description>
   --<parameter name="V1">The first vector</parameter>
   --<parameter name="V2">The second vector, subtracted from the first vector</parameter>
   --<exception>None at this moment</exception>

   function "-" (P1, P2 : in Point_3D) return Vector_3D;
   --<summary>Returns the vector from P2 to P1</summary>
   --<description>Returns the vector from P2 to P1</description>
   --<parameter name="P1">The first point</parameter>
   --<parameter name="P2">The second point</parameter>
   --<exception>None at this moment</exception>

   function "*" (V : in Vector_3D; S : in Large_Float) return Vector_3D;
   --<summary>Multiplies a Vector_3D by a Scalar</summary>
   --<description>function to multiply a Vector_3D by a Scalar</description>
   --<parameter name="V">The vector to mulitply</parameter>
   --<parameter name="S">The factor to multiply the vector with</parameter>
   --<exception>None at this moment</exception>

   function "*" (S : in Large_Float; V : in Vector_3D) return Vector_3D;
   --<summary>Multiplies a Vector_3D by a Scalar</summary>
   --<description>function to multiply a Vector_3D by a Scalar</description>
   --<parameter name="S">The factor to multiply the vector with</parameter>
   --<parameter name="V">The vector to mulitply</parameter>
   --<exception>None at this moment</exception>

   function "*" (V1 : in Vector_3D; V2 : in Vector_3D) return Large_Float;
   --<summary>Computes the dot-product of two Vector_3D variables</summary>
   --<description>function to compute the dot-product of two Vector_3D variables</description>
   --<parameter name="V1">The first vector</parameter>
   --<parameter name="V2">The second vector</parameter>
   --<exception>None at this moment</exception>

   function "*" (M : in Matrix_3D; V : in Vector_3D) return Vector_3D;
   --<summary>Transforms a Vector_3D with a Matrix_3D</summary>
   --<description>function to compute a transformed vector with a matrix as in V' := M * V, excluding the Translation,
   --as vectors to not translate, as opposed to points</description>
   --<parameter name="M">The matrix</parameter>
   --<parameter name="V">The vector</parameter>
   --<exception>None at this moment</exception>

   function "**" (V1 : in Vector_3D; V2 : in Vector_3D) return Vector_3D;
   --<summary>Computes the cross-product of two Vector_3D variables</summary>
   --<description>function to compute the cross-product of two Vector_3D variables</description>
   --<parameter name="V1">The first vector</parameter>
   --<parameter name="V2">The second vector</parameter>
   --<exception>None at this moment</exception>

   function Length (V : in Vector_3D) return Large_Float;
   --<summary>Returns the length of a Vector_3D</summary>
   --<description>function to compute the length of a Vector_3D</description>
   --<parameter name="V">The vector</parameter>
   --<exception>None at this moment</exception>

   function Length_Squared (V : in Vector_3D) return Large_Float;
   --<summary>Returns the squared length of a Vector_3D</summary>
   --<description>function to compute the squared length of a Vector_3D</description>
   --<parameter name="V">The vector</parameter>
   --<exception>None at this moment</exception>

   function Normalize (V : in Vector_3D) return Vector_3D;
   --<summary>Computes the normalized version of a Vector_3D</summary>
   --<description>function to compute the normalized version of a Vector_3D</description>
   --<parameter name="V">The vector to normalize</parameter>
   --<exception>None at this moment</exception>

   function To_Vector_3D (N : in Normal_3D) return Vector_3D;
   --<summary>Converts a Normal_3D into a Vector_3D</summary>
   --<description>function to convert a Normal_3D into a Vector_3D</description>
   --<parameter name="N">The normal to convert</parameter>
   --<exception>None at this moment</exception>

   function To_Vector_3D (P : in Point_3D) return Vector_3D;
   --<summary>Converts a Point_3D into a Vector_3D</summary>
   --<description>function to convert a Point_3D into a Vector_3D</description>
   --<parameter name="P">The point to convert</parameter>
   --<exception>None at this moment</exception>

   -----------------
   --  Normal_3D ADT
   -----------------

   function Construct_Normal (X, Y, Z : in Large_Float) return Normal_3D;
   --<summary>Constructs a Normal_3D</summary>
   --<description>Constructs a Normal_3D with the coordinates passed in. The origin is the default coordinate</description>
   --<parameter name="X">The X-coordinate of the point</parameter>
   --<parameter name="Y">The Y-coordinate of the point</parameter>
   --<parameter name="Z">The Y-coordinate of the point</parameter>
   --<exception>None at this moment</exception>

   procedure Put (N : in Normal_3D; Msg : in String := "Normal_3D = ");
   --<summary>Prints a Normal_3D</summary>
   --<description>Prints a Normal_3D to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="N">The normal to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_X (N : in Normal_3D) return Large_Float;
   --<summary>Returns the X-coordinate of a Normal_3D</summary>
   --<description>Returns the X-coordinate of a Normal_3D</description>
   --<parameter name="N">The normal to return the X-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Y (N : in Normal_3D) return Large_Float;
   --<summary>Returns the Y-coordinate of a Normal_3D</summary>
   --<description>Returns the Y-coordinate of a Normal_3D</description>
   --<parameter name="N">The normal to return the Y-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function Get_Z (N : in Normal_3D) return Large_Float;
   --<summary>Returns the Z-coordinate of a Normal_3D</summary>
   --<description>Returns the Z-coordinate of a Normal_3D</description>
   --<parameter name="N">The normal to return the Z-coordinate from</parameter>
   --<exception>None at this moment</exception>

   function "-" (N : in Normal_3D) return Normal_3D;
   --<summary>Negates a Normal_3D</summary>
   --<description>function to compute the negative (inverse) version of a Normal_3D</description>
   --<parameter name="N">The normal to negate</parameter>
   --<exception>None at this moment</exception>

   function "+" (N1 : in Normal_3D; N2 : in Normal_3D) return Normal_3D;
   --<summary>Adds two Normal_3D variables</summary>
   --<description>function to add two Normal_3D variables</description>
   --<parameter name="N1">The first normal</parameter>
   --<parameter name="N2">The second normal</parameter>
   --<exception>None at this moment</exception>

   function "-" (N1 : in Normal_3D; N2 : in Normal_3D) return Normal_3D;
   --<summary>Subtracts two Normal_3D variables</summary>
   --<description>function to subtract two Normal_3D variables</description>
   --<parameter name="N1">The first normal</parameter>
   --<parameter name="N2">The second normal, subtracted from the first normal</parameter>
   --<exception>None at this moment</exception>

   function "*" (N1 : in Normal_3D; N2 : in Normal_3D) return Large_Float;
   --<summary>Computes the dot-product of two Normal_3D variables</summary>
   --<description>function to compute the dot-product of two Normal_3D variables</description>
   --<parameter name="N1">The first normal</parameter>
   --<parameter name="N2">The second normal</parameter>
   --<exception>None at this moment</exception>

   function "*" (M : in Matrix_3D; N : in Normal_3D) return Normal_3D;
   --<summary>Transforms a Normal_3D with a transposed version of Matrix_3D</summary>
   --<description>function to compute a transformed normal with a matrix as in N' := M * N, excluding the Translation,
   --as vectors to not translate, as opposed to points. The transposed matrix is used in the calculation, as
   --this is needed to transform a normal back to world coordinates</description>
   --<parameter name="M">The matrix</parameter>
   --<parameter name="N">The vector</parameter>
   --<exception>None at this moment</exception>

   function "**" (N1 : in Normal_3D; N2 : in Normal_3D) return Normal_3D;
   --<summary>Computes the cross-product of two Normal_3D variables</summary>
   --<description>function to compute the cross-product of two Normal_3D variables</description>
   --<parameter name="N1">The first normal</parameter>
   --<parameter name="N2">The second normal</parameter>
   --<exception>None at this moment</exception>

   function Normalize (N : in Normal_3D) return Normal_3D;
   --<summary>Computes the normalized version of a Normal_3D</summary>
   --<description>function to compute the normalized version of a Normal_3D</description>
   --<parameter name="N">The normal to normalize</parameter>
   --<exception>None at this moment</exception>

   function To_Normal_3D (V : in Vector_3D) return Normal_3D;
   --<summary>Converts a Vector_3D into a Normal_3D</summary>
   --<description>function to convert a Vector_3D into a Normal_3D</description>
   --<parameter name="V">The vector to convert</parameter>
   --<exception>None at this moment</exception>

   function To_Normal_3D (P : in Point_3D) return Normal_3D;
   --<summary>Converts a Point_3D into a Normal_3D</summary>
   --<description>function to convert a Vector_3D into a Normal_3D</description>
   --<parameter name="P">The point to convert</parameter>
   --<exception>None at this moment</exception>

   -----------------
   --  Matrix_3D ADT
   -----------------

   procedure Put (M : in Matrix_3D; Msg : in String := "Matrix_3D = ");
   --<summary>Prints a Matrix_3D</summary>
   --<description>Prints a Matrix_3D to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="M">The matrix to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Construct_Translation (Tx, Ty, Tz : in Large_Float) return Matrix_3D;
   --<summary>Constructs a Translation Matrix_3D by three coordinates</summary>
   --<description>Constructs a Translation Matrix_3D by three coordinates</description>
   --<parameter name="Tx">The X-amount to translate with</parameter>
   --<parameter name="Ty">The Y-amount to translate with</parameter>
   --<parameter name="Tz">The Z-amount to translate with</parameter>
   --<exception>None at this moment</exception>

   function Construct_Translation (To : in Point_3D) return Matrix_3D;
   --<summary>Constructs a Translation Matrix_3D by a point</summary>
   --<description>Constructs a Translation Matrix_3D by a point</description>
   --<parameter name="To">The point to translate to</parameter>
   --<exception>None at this moment</exception>

   function Construct_Translation (To : in Vector_3D) return Matrix_3D;
   --<summary>Constructs a Translation Matrix_3D by a vector</summary>
   --<description>Constructs a Translation Matrix_3D by a vector</description>
   --<parameter name="To">The vector to translate with</parameter>
   --<exception>None at this moment</exception>

   function Construct_Scale (Sx, Sy, Sz : in Large_Float) return Matrix_3D;
   --<summary>Constructs a Scaling Matrix_3D</summary>
   --<description>Constructs a Scaling Matrix_3D</description>
   --<parameter name="Sx">The amount to scale with along the X-axis</parameter>
   --<parameter name="Sy">The amount to scale with along the Y-axis</parameter>
   --<parameter name="Sz">The amount to scale with along the Z-axis</parameter>
   --<exception>None at this moment</exception>

   function Construct_Shear (Hxy, Hxz, Hyx, Hyz, Hzx, Hzy : in Large_Float) return Matrix_3D;
   --<summary>Constructs a Shearing Matrix_3D</summary>
   --<description>Constructs a Shearing Matrix_3D</description>
   --<parameter name="Hxy">The amount to shear with along the XY-plane</parameter>
   --<parameter name="Hxz">The amount to shear with along the XZ-plane</parameter>
   --<parameter name="Hyx">The amount to shear with along the YX-plane</parameter>
   --<parameter name="Hyz">The amount to shear with along the YZ-plane</parameter>
   --<parameter name="Hzx">The amount to shear with along the ZX-plane</parameter>
   --<parameter name="Hzy">The amount to shear with along the ZY-plane</parameter>
   --<exception>None at this moment</exception>

   function Construct_Rotate_X (Angle : in Large_Float) return Matrix_3D;
   --<summary>Constructs a rotation Matrix_3D around the X-axis</summary>
   --<description>Constructs a rotation Matrix_3D around the X-axis</description>
   --<parameter name="Angle">The amount to rotate along the X-axis</parameter>
   --<exception>None at this moment</exception>

   function Construct_Rotate_Y (Angle : in Large_Float) return Matrix_3D;
   --<summary>Constructs a rotation Matrix_3D around the Y-axis</summary>
   --<description>Constructs a rotation Matrix_3D around the Y-axis</description>
   --<parameter name="Angle">The amount to rotate along the Y-axis</parameter>
   --<exception>None at this moment</exception>

   function Construct_Rotate_Z (Angle : in Large_Float) return Matrix_3D;
   --<summary>Constructs a rotation Matrix_3D around the Z-axis</summary>
   --<description>Constructs a rotation Matrix_3D around the Z-axis</description>
   --<parameter name="Angle">The amount to rotate along the Z-axis</parameter>
   --<exception>None at this moment</exception>

   function "*" (M1, M2 : in Matrix_3D) return Matrix_3D;
   --<summary>Compute the product of two matrices as in M := M1 * M2</summary>
   --<description>Compute the product of two matrices as in M := M1 * M2.
   --Mathematically it means that M1 is applied after M2: M(p) = M1(M2(p))</description>
   --<parameter name="M1">The first matrix</parameter>
   --<parameter name="M2">The second matrix</parameter>
   --<exception>None at this moment</exception>

   function Inverse (M : in Matrix_3D) return Matrix_3D;
   --<summary>Computes the inverse of a Matrix_3D</summary>
   --<description>Computes the inverse of a Matrix_3D</description>
   --<parameter name="M">The matrix to inverse</parameter>
   --<exception>None at this moment</exception>

   -----------
   --  Ray ADT
   -----------

   function Construct_Ray (Org : in Point_3D; Dir : in Vector_3D) return Ray;
   --<summary>Constructs a Ray</summary>
   --<description>Constructs a Ray with origin Org and direction Dir</description>
   --<parameter name="Org">Origin of the ray</parameter>
   --<parameter name="Dir">Direction of the ray</parameter>
   --<exception>None at this moment</exception>

   procedure Put (R : in Ray; Msg : in String := "Ray = ");
   --<summary>Prints a Ray</summary>
   --<description>Prints a Ray to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="R">The ray to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_Origin (R : in Ray) return Point_3D;
   --<summary>Returns the origin of a Ray</summary>
   --<description>Returns the origin of a Ray</description>
   --<parameter name="R">The ray to return the origin of</parameter>
   --<exception>None at this moment</exception>

   function Get_Direction (R : in Ray) return Vector_3D;
   --<summary>Returns the direction of a Ray</summary>
   --<description>Returns the direction  of a Ray</description>
   --<parameter name="R">The ray to return the direction of</parameter>
   --<exception>None at this moment</exception>

   function "*" (M : in Matrix_3D; R : in Ray) return Ray;
   --<summary>Transforms a Ray with a Matrix_3D</summary>
   --<description>function to compute a transformed ray with a matrix as in R' := M * R</description>
   --<parameter name="M">The matrix</parameter>
   --<parameter name="R">The vector</parameter>
   --<exception>None at this moment</exception>

   function Get_Lambda_For_Point (R : in Ray; P : in Point_3D) return Large_Float;
   --<summary>Returns the lambda for a point on a Ray</summary>
   --<description>Returns the lambda for a point on a Ray</description>
   --<parameter name="R">The ray on which the point is laying</parameter>
   --<parameter name="P">The point to get the lambda for</parameter>
   --<exception>None at this moment</exception>

   --
   --  Some useful constants
   --
   X_AXIS_3D : constant Vector_3D;
   Y_AXIS_3D : constant Vector_3D;
   Z_AXIS_3D : constant Vector_3D;
   ORIGIN_3D : constant Point_3D;

   X_NORM_3D : constant Normal_3D;
   Y_NORM_3D : constant Normal_3D;
   Z_NORM_3D : constant Normal_3D;

private

   type Point_2D is record
      X : Large_Float := 0.0;
      Y : Large_Float := 0.0;
   end record;

   type Point_3D is record
      X : Large_Float := 0.0;
      Y : Large_Float := 0.0;
      Z : Large_Float := 0.0;
   end record;

   type Vector_3D is record
      X : Large_Float := 0.0;
      Y : Large_Float := 0.0;
      Z : Large_Float := 0.0;
   end record;

   type Normal_3D is record
      X : Large_Float := 0.0;
      Y : Large_Float := 0.0;
      Z : Large_Float := 1.0;
   end record;

   type Matrix_3D is record
      X2, X3, Tx, Y1, Y3, Ty, Z1, Z2, Tz : Large_Float := 0.0;
      X1, Y2, Z3                         : Large_Float := 1.0;
   end record;

   type Ray is record
      Org : Point_3D;
      Dir : Vector_3D;
   end record;

   X_AXIS_3D : constant Vector_3D := (X => 1.0, Y => 0.0, Z => 0.0);
   Y_AXIS_3D : constant Vector_3D := (X => 0.0, Y => 1.0, Z => 0.0);
   Z_AXIS_3D : constant Vector_3D := (X => 0.0, Y => 0.0, Z => 1.0);
   ORIGIN_3D : constant Point_3D  := (X => 0.0, Y => 0.0, Z => 0.0);
   X_NORM_3D : constant Normal_3D := (X => 1.0, Y => 0.0, Z => 0.0);
   Y_NORM_3D : constant Normal_3D := (X => 0.0, Y => 1.0, Z => 0.0);
   Z_NORM_3D : constant Normal_3D := (X => 0.0, Y => 0.0, Z => 1.0);

end Linear_Math;

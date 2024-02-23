--<summary>The Unit_Cylinders package contains the Abstract Data Type for ray tracing of a Unit Cylinder</summary>
--<description>The Unit_Cylinders package contains the Abstract Data Type for ray tracing of a Unit Cylinder.
--Unit cylinders are cylinders with a radius of 1 and centre at the origin. Unit cylinders can be transformed using affine
--transformation. Rays will be transformed to the unit cylinder object coordinate system via the inverse object transformation.
--</description>
--<group>Objects</group>
package Objects.Unit_Cylinders is

   ------------------
   -- ADT Unit_Cylinder
   ------------------

   type Unit_Cylinder is tagged private;
   --<summary>ADT for the unit sphere</summary>

   function Construct_Unit_Cylinder (Name : in String) return Object_Ptr;
   --<summary>Creates a Unit_Cylinder object</summary>
   --<description>Function that creates a Unit_Cylinder object</description>
   --<parameter name="Name">The Name of the Unit_Cylinder</parameter>
   --<exception>None at this moment</exception>

   function Construct_Positioned_Cylinder (Name : in String; P1, P2 : in Point_3D; Radius : Large_Float) return Object_Ptr;
   --<summary>Creates a cylinder with a specific radius, start point and end point</summary>
   --<description>Creates a cylinder with a specific radius, start point and end point</description>
   --<parameter name="Name">The Name of the Unit_Cylinder</parameter>
   --<parameter name="P1">The start point of the Cylinder</parameter>
   --<parameter name="P2">The end point of the Cylinder</parameter>
   --<parameter name="Radius">The radius of the Cylinder</parameter>
   --<exception>None at this moment</exception>

   function Intersect (Cylinder : in Unit_Cylinder; R : in Ray) return Natural;
   --<summary>intersects a Unit_Cylinder with a Ray</summary>
   --<description>Procedure to intersect a unit sphere with a ray. It adds the hitpoints to the hitpoint list</description>
   --<parameter name="Cylinder">A Unit_Cylinder</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Cylinder : in Unit_Cylinder; R : in Ray) return Natural;
   --<summary>Hits a Unit_Cylinder with a Ray</summary>
   --<description>Procedure to hit a unit sphere with a ray. It adds the hitpoints to the hitpoint list.
   --It is used to trace shadow rays, for which normals and hitpoint coordinates are less relevant.
   --Hence the code is a simplified version of Intersect.</description>
   --<parameter name="Cylinder">A Unit_Cylinder</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

private
   type Unit_Cylinder is new Object with null record;
   --<summary>Unit_Cylinder is a child of the Class-wide type Object</summary>

end Objects.Unit_Cylinders;

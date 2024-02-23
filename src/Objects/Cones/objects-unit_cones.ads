--<summary>The Unit_Cones package contains the Abstract Data Type for ray tracing of a Unit Cone</summary>
--<description>The Unit_Cones package contains the Abstract Data Type for ray tracing of a Unit Cone.
--Unit cones are cones with a radius of 1 and centre at the origin. Unit cones can be transformed using affine
--transformation. Rays will be transformed to the unit cone object coordinate system via the inverse object transformation.
--</description>
--<group>Objects</group>
package Objects.Unit_Cones is

   ----------------
   -- ADT Unit_Cone
   ----------------

   type Unit_Cone is tagged private;
   --<summary>ADT for the unit sphere</summary>

   function Construct_Unit_Cone (Name : in String) return Object_Ptr;
   --<summary>Creates a Unit_Cone object</summary>
   --<description>Function that creates a Unit_Cone object</description>
   --<parameter name="Name">The Name of the Unit_Cone</parameter>
   --<exception>None at this moment</exception>

   function Construct_Positioned_Cone (Name : in String; P1, P2 : in Point_3D; Radius : Large_Float) return Object_Ptr;
   --<summary>Creates a cone with a specific base radius, start point and end point</summary>
   --<description>Creates a cone with a specific base radius, start point and end point</description>
   --<parameter name="Name">The Name of the Cone</parameter>
   --<parameter name="P1">The center point of the base of the Cone</parameter>
   --<parameter name="P2">The end point of the Cone</parameter>
   --<parameter name="Radius">The base radius of the Cone</parameter>
   --<exception>None at this moment</exception>

   function Intersect (Cone : in Unit_Cone; R : in Ray) return Natural;
   --<summary>intersects a Unit_Cone with a Ray</summary>
   --<description>Procedure to intersect a unit cone with a ray. It adds the hitpoints to the hitpoint list</description>
   --<parameter name="Cone">A Unit_Cone</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Cone : in Unit_Cone; R : in Ray) return Natural;
   --<summary>Hits a Unit_Cone with a Ray</summary>
   --<description>Procedure to hit a unit sphere with a ray. It adds the hitpoints to the hitpoint list.
   --It is used to trace shadow rays, for which normals and hitpoint coordinates are less relevant.
   --Hence the code is a simplified version of Intersect.</description>
   --<parameter name="Cone">A Unit_Cone</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

private
   type Unit_Cone is new Object with null record;
   --<summary>Unit_Cone is a child of the Class-wide type Object</summary>

end Objects.Unit_Cones;

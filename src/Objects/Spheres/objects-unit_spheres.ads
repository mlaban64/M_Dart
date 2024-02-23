--<summary>The Unit_Spheres package contains the Abstract Data Type for ray tracing of a Unit Sphere</summary>
--<description>The Unit_Spheres package contains the Abstract Data Type for ray tracing of a Unit Sphere.
--Unit spheres are spheres with a radius of 1 and centre at the origin. Unit spheres can be transformed using affine
--transformation. Rays will be transformed to the unit sphere object coordinate system via the inverse object transformation.
--</description>
--<group>Objects</group>
package Objects.Unit_Spheres is

   ------------------
   -- ADT Unit_Sphere
   ------------------

   type Unit_Sphere is tagged private;
   --<summary>ADT for the unit sphere</summary>

   function Construct_Unit_Sphere (Name : in String) return Object_Ptr;
   --<summary>Creates a Unit_Sphere object</summary>
   --<description>Function that creates a Unit_Sphere object</description>
   --<parameter name="Name">The Name of the Unit_Sphere</parameter>
   --<exception>None at this moment</exception>

   function Construct_Positioned_Sphere (Name : in String; P : in Point_3D; Radius : Large_Float) return Object_Ptr;
   --<summary>Creates a sphere with a specific radius and center point</summary>
   --<description>Creates a sphere with a specific radius and center point</description>
   --<parameter name="Name">The Name of the Sphere</parameter>
   --<parameter name="P">The start point of the Sphere</parameter>
   --<parameter name="Radius">The radius of the Sphere</parameter>
   --<exception>None at this moment</exception>

   function Intersect (Sphere : in Unit_Sphere; R : in Ray) return Natural;
   --<summary>intersects a Unit_Sphere with a Ray</summary>
   --<description>Procedure to intersect a unit sphere with a ray. It adds the hitpoints to the hitpoint list</description>
   --<parameter name="Sphere">A Unit_Sphere</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Sphere : in Unit_Sphere; R : in Ray) return Natural;
   --<summary>Hits a Unit_Sphere with a Ray</summary>
   --<description>Procedure to hit a unit sphere with a ray. It adds the hitpoints to the hitpoint list.
   --It is used to trace shadow rays, for which normals and hitpoint coordinates are less relevant.
   --Hence the code is a simplified version of Intersect.</description>
   --<parameter name="Sphere">A Unit_Sphere</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

private
   type Unit_Sphere is new Object with null record;
   --<summary>Unit_Sphere is a child of the Class-wide type Object</summary>

end Objects.Unit_Spheres;

--<summary>The Triangles package contains the Abstract Data Type for ray tracing of a Triangle</summary>
--<description>The Triangles package contains the Abstract Data Type for ray tracing of a Triangles.
--Triangles are stored by 3 vertices. The normal is defined by going counter-clockwise through the vertices. Triangles can be transformed using affine
--transformation. Rays will be transformed to the triangle coordinate system via the inverse object transformation.
--</description>
--<group>Objects</group>

package Objects.Triangles is

   ------------------
   -- ADT Triangle
   ------------------

   type Triangle is tagged private;
   --<summary>ADT for the triangle</summary>

   type Triangle_Ptr is access all Triangle'Class;
   --<summary>The access type to all CSG_Object'Class objects</summary>

   function Construct_Triangle (Name : in String; V1, V2, V3 : in Point_3D) return Object_Ptr;
   --<summary>Creates a Triangle by three vertices</summary>
   --<description>Creates a Triangle by three vertices</description>
   --<parameter name="Name">The Name of the Triangle</parameter>
   --<parameter name="V1">The first vertex of the triangle</parameter>
   --<parameter name="V2">The second vertex of the triangle</parameter>
   --<parameter name="V3">The third vertex of the triangle</parameter>
   --<exception>None at this moment</exception>

   function Intersect (Trgl : in Triangle; R : in Ray) return Natural;
   --<summary>intersects a Triangle with a Ray</summary>
   --<description>Procedure to intersect a triangle with a ray. It adds the hitpoints to the hitpoint list</description>
   --<parameter name="Trgl">A Triangle</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Trgl : in Triangle; R : in Ray) return Natural;
   --<summary>Hits a Triangle with a Ray</summary>
   --<description>Procedure to hit a triange with a ray. It adds the hitpoints to the hitpoint list.
   --It is used to trace shadow rays, for which normals and hitpoint coordinates are less relevant.
   --Hence the code is a simplified version of Intersect.</description>
   --<parameter name="Trgl">A Triangle</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

private

   type Triangle is new Object with record
      V1, V2, V3       : Point_3D;
      Normal           : Normal_3D;
      A, B, E, F, I, J : Large_Float; -- variables used in the intersection calculations that can be computed during construction
   end record;
   --<summary>Triangle is a child of the Class-wide type Object</summary>

end Objects.Triangles;

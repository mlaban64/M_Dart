--<summary>The Unit_Cubes package contains the Abstract Data Type for ray tracing of a Unit Cube</summary>
--<description>The Unit_Cubes package contains the Abstract Data Type for ray tracing of a Unit Cube.
--Unit cubes are cubes with oposite corners at (-1,-1,-1) and (1,1,1). Unit cubes can be transformed using affine
--transformation. Rays will be transformed to the unit cube object coordinate system via the inverse object transformation.
--</description>
--<group>Objects</group>
package Objects.Unit_Cubes is

   ----------------
   -- ADT Unit_Cube
   ----------------

   type Unit_Cube is tagged private;
   --<summary>ADT for the unit cube</summary>

   function Construct_Unit_Cube (Name : in String) return Object_Ptr;
   --<summary>Creates a Unit_Cube object</summary>
   --<description>Function that creates a Unit_Cube object</description>
   --<parameter name="Name">The Name of the Unit_Cube</parameter>
   --<exception>None at this moment</exception>

   function Get_Normal_For_Cube3D (Face : Integer) return Normal_3D;
   --  <summary>Returns the Normal for a face of a unit cube</summary> <parameter name="Face">an integer number for the
   --  face</parameter> <exception>No exception</exception>

   function Intersect (Cube : in Unit_Cube; R : in Ray) return Natural;
   --<summary>intersects a Unit_Cube with a Ray and returns the number of hitpoints pushed on the stack</summary>
   --<description>Procedure to intersect a unit cube with a ray and returns the number of hitpoints pushed on the stack</description>
   --<parameter name="Cube">A Unit_Cube</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

   function Hit (Cube : in Unit_Cube; R : in Ray) return Natural;
   --<summary>Hits a Unit_Cube with a Ray</summary>
   --<description>Procedure to hit a unit sphere with a ray. It adds the hitpoints to the hitpoint list.
   --It is used to trace shadow rays, for which normals and hitpoint coordinates are less relevant.
   --Hence the code is a simplified version of Intersect.</description>
   --<parameter name="Sphere">A Unit_Cube</parameter>
   --<parameter name="R">A Ray that is intersected with the object</parameter>
   --<exception>None at this moment</exception>

private
   type Unit_Cube is new Object with null record;
   --<summary>Unit_Cube is a child of the Class-wide type Object</summary>

end Objects.Unit_Cubes;

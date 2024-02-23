--  <description>
--  Package Cubes3D contains the derived type and subprograms to handle 3D Cubes
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes;              use StdTypes;
with Math3D;                use Math3D;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Objects3D;             use Objects3D;

package Cubes3D is

   ------------------------
   --  TYPE DEFINITIONS  --
   ------------------------

   type Cube3D is limited private;
   --  <summary>Cube3D represents 3D solid unit cube
   --  meaning a cube with sides of length 2 and its centre at (0,0,0) </summary>

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructCube3D (ObjName : in Unbounded_String) return Object3D_Ptr;
   --  <summary>function to construct a Cube3D</summary>
   --  <parameter name="ObjName">An unbounded string, thge name of the object</parameter>
   --  <exception>No exception</exception>

   ---------------------------------------
   --  CLASS-WIDE CUBE3D SUBPROGRAMS  --
   ---------------------------------------
   procedure Intersect(Obj3D : in Cube3D; Ray : in Ray3D; HP_List: in out HitPoint3D_List);
   --  <summary>Intersect tries to intersect a Cube3D by a Ray3D</summary>
   --  <parameter name="Obj3D">An object of the Object3D Class Wide Type</parameter>
   --  <parameter name="Ray">A Ray that is intersected with Obj3D</parameter>
   --  <parameter name="HP_List">The HitPoint List to which any hitpoints are added</parameter>
   --  <exception>No exception</exception>

   ------------------------------
   --  SUPPORTING SUBPROGRAMS  --
   ------------------------------
   function GetNormalCube3D(face: Integer) return Normal3D;
   --  <summary>Returns the Normal for a face of a unit cube</summary>
   --  <parameter name="face">an integer number for the face</parameter>
   --  <exception>No exception</exception>

private

   type Cube3D is new Object3D with null record;

end Cubes3D;

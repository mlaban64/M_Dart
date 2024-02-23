--  <description>
--  Package Spheres3D contains the derived type and subprograms to handle 3D Spheres
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes;              use StdTypes;
with Math3D;                use Math3D;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Objects3D;             use Objects3D;

package Spheres3D is

   ------------------------
   --  TYPE DEFINITIONS  --
   ------------------------

   type Sphere3D is limited private;
   --  <summary>Sphere3D represents 3D solid unit sphere
   --  meaning a sphere with a radius of 1 and its centre at (0,0,0) </summary>

   -------------------------------
   --  CONSTRUCTOR SUBPROGRAMS  --
   -------------------------------

   function ConstructSphere3D (ObjName : in Unbounded_String) return Object3D_Ptr;
   --  <summary>function to construct a Sphere3D</summary>
   --  <parameter name="ObjName">An unbounded string, thge name of the object</parameter>
   --  <exception>No exception</exception>

   ---------------------------------------
   --  CLASS-WIDE SPHERE3D SUBPROGRAMS  --
   ---------------------------------------
   procedure Intersect(Obj3D : in Sphere3D; Ray : in Ray3D; HP_List: in out HitPoint3D_List);
   --  <summary>Intersect tries to intersect a Sphere3D by a Ray3D</summary>
   --  <parameter name="Obj3D">The object of the Object3D Class Wide Type</parameter>
   --  <parameter name="Ray">A Ray that is intersected with Obj3D</parameter>
   --  <parameter name="HP_List">The HitPoint List to which any hitpoints are added</parameter>
   --  <exception>No exception</exception>

private

   type Sphere3D is new Object3D with null record;
   --  <summary>Sphere3D type is a 3D Solid Sphere</summa

end Spheres3D;

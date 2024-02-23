with Core_Types;  use Core_Types;
with Linear_Math; use Linear_Math;

--<summary>The HitPoints package contains all Abstract Data Types that handle hitpoints between a ray and an object</summary>
--<description>The HitPoints package contains all Abstract Data Types that handle hitpoints. Hitpoints are used to store
--  the geometrical data of an intersection between a ray and an object. No references to materials, objects or else are stored,
--  to avoid cyclic dependencies. Since an object can have many different intersections with a ray, a stack is emulated to store
--  hitpoints.
--</description>
--<group>Objects</group>
package HitPoints is

   -------------------------------------------
   --  ADT HitPoint
   -------------------------------------------

   type HitPoint is private;
   --  <summary>ADT representing a HitPoint in 3D</summary>

   type HitPoint_Stack is private;
   --  <summary>ADT representing a HitPoint Stack</summary>
   -------------------------
   -- ADT HitPoint Functions
   -------------------------

   procedure Push_HitPoint (Lambda : in Large_Float; R : in Ray; ObjectHp : in Point_3D; ObjectNv : in Normal_3D);
   --<summary>Creates a new HitPoint with the values passed in and puts it on the stack</summary>
   --<description>Creates a new HitPoint with the values passed in and puts it on the stack</description>
   --<parameter name="Lambda">The Lambda parameter of the ray of the hitpoint</parameter>
   --<parameter name="R">The Ray in world coordinates that generates the hitpoint</parameter>
   --<parameter name="ObjectHp">The hitpoint in object coordinates</parameter>
   --<parameter name="ObjectNv">The normal in object coordinates</parameter>
   --<exception>None at this moment</exception>

   function Pop_HitPoint return HitPoint;
   --<summary>Pops the top hitpoint from the stack</summary>
   --<description>Pops the top hitpoint from the stack</description>
   --<exception>None at this moment</exception>

   procedure Put (Hp : in HitPoint; Msg : in String := "HitPoint ");
   --<summary>Prints an HitPoint's statistics</summary>
   --<description>Prints an HitPoint's statistics</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_Lambda (Hp : in HitPoint) return Large_Float;
   --<summary>Returns the Lambda of a hitpoint</summary>
   --<description>Function to return the Lambda of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_Ray (Hp : in HitPoint) return Ray;
   --<summary>Returns the Ray of a hitpoint</summary>
   --<description>Function to return the Ray of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_ObjectHp (Hp : in HitPoint) return Point_3D;
   --<summary>Returns the ObjectHp of a hitpoint</summary>
   --<description>Function to return the ObjectHp of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_ObjectNv (Hp : in HitPoint) return Normal_3D;
   --<summary>Returns the ObjectNv of a hitpoint</summary>
   --<description>Function to return the ObjectNv of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_WorldHp (Hp : in HitPoint) return Point_3D;
   --<summary>Returns the WorldHp of a hitpoint</summary>
   --<description>Function to return the WorldHp of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   function Get_WorldNv (Hp : in HitPoint) return Normal_3D;
   --<summary>Returns the WorldNv of a hitpoint</summary>
   --<description>Function to return the WorldNv of a hitpoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

   procedure Set_WorldHp (Hp : in out HitPoint; WorldHp : in Point_3D);
   --<summary>Sets the HitPoint WorldHp</summary>
   --<description>Sets the HitPoint WorldHp</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<parameter name="WorldHp">hit point in world coordinates</parameter>
   --<exception>None at this moment</exception>

   procedure Set_WorldNv (Hp : in out HitPoint; WorldNv : in Normal_3D);
   --<summary>Sets the HitPoint WorldNv</summary>
   --<description>Sets the HitPoint WorldNv</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<parameter name="WorldNv">normal in world coordinates</parameter>
   --<exception>None at this moment</exception>

   procedure Flip_Normals (Hp : in out HitPoint);
   --<summary>Flips the normals of the HitPoint</summary>
   --<description>Flips the normals of the HitPoint</description>
   --<parameter name="Hp">a HitPoint</parameter>
   --<exception>None at this moment</exception>

private

   type HitPoint is record
      R : Ray;
      --  Ray in world coordinates
      Object_Hp : Point_3D;
      --  Object_Hp is the actual hitpoint in local object coordinates;
      World_Hp : Point_3D;
      --  WorldHP is the actual hitpoint in world coordinates;
      Object_Nv : Normal_3D;
      --  Normal at ObjectHp in local object coordinates;
      World_Nv : Normal_3D;
      --  Normal at WorldP;
      Lambda : Large_Float := 0.0;
      --  The Lambda parameter of the ray as in HP := Ray.Org + Lambda*Ray.Dir
   end record;

   MAX_HP_STACK : constant Natural := 1000;
   --  Max depth of the hitpoint stack. 1000 is pretty arbitrary but seems high enough

   type HitPoint_Array is array (1 .. MAX_HP_STACK) of HitPoint;

   type HitPoint_Stack is record
      StackPointer : Integer := 0;
      --  StackPointer always points to the object on the stack, but is 0 when nothing is put on the stack yet
      Stack : HitPoint_Array;
   end record;

   Hp_Stack : HitPoint_Stack;
   --  The global Hitpoint Stack

end HitPoints;

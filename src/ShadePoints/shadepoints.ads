with Core_Types;            use Core_Types;
with Linear_Math;           use Linear_Math;
with HitPoints;             use HitPoints;
with Objects;               use Objects;
with Spectra;               use Spectra;
with Utilities;             use Utilities;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The ShadePoints package contains all Abstract Data Types to manage ShadePoints</summary>
--<description>The ShadePoints package contains all Abstract Data Types to manage ShadePoints.
--A ShadePoint is an object that contains all the information to compute the radiance of a point.
--It contains all relevant information to compute (in)direct illumination and spawn secondary rays</description >
--<group>Rendering</group>
package ShadePoints is

   ------------------
   --  ADT ShadePoint
   ------------------

   type ShadePoint_List is private;
   --  <summary>ADT of the root type of ShadePoint Lists. It defaults to an empty list, with no elements (Count = 0)</summary>

   type ShadePoint_List_Element is private;
   --  <summary>ADT of a linked list element of pointers to ShadePoint objects</summary>

   type ShadePoint is private;
   --  <summary>ADT representing a ShadePoint</summary>

   type ShadePoint_Ptr is access all ShadePoint;
   --  <summary>The access type to a ShadePoint</summary>

   type ShadePoint_List_Element_Ptr is access all ShadePoint_List_Element;
   --  <summary>The access type to an ShadePoint_List_Element</summary>

   -------------------------
   -- ADT ShadePoint Functions
   -------------------------

   function Construct_ShadePoint (Hp : HitPoint; Obj_Ptr : in Object_Ptr) return ShadePoint_Ptr;
   --<summary>Creates a new ShadePoint with the values passed in and return its pointer</summary>
   --<description>Creates a new ShadePoint with the values passed in and return its pointer</description>
   --<parameter name="Hp">The hitpoint of the ray</parameter>
   --<parameter name="Obj_Ptr">The object being hit</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Sp_Ptr : in ShadePoint_Ptr; Msg : in String := "ShadePoint ");
   --<summary>Prints an ShadePoint's statistics</summary>
   --<description>Prints an ShadePoint's statistics</description>
   --<parameter name="Sp_Ptr">A pointer to a ShadePoint</parameter>
   --<parameter name="Msg">An optional message</parameter>
   --<exception>None at this moment</exception>

   function Get_HitPoint (Sp_Ptr : in ShadePoint_Ptr) return HitPoint;
   --<summary>Returns the HitPoint of a ShadePoint</summary>
   --<description>Function to return the HitPoint of a ShadePoint</description>
   --<parameter name="Sp_Ptr">Pointer to a ShadePoint</parameter>
   --<exception>None at this moment</exception>

   procedure Compute_Hitpoint_World (Sp_Ptr : in ShadePoint_Ptr; Extra_Flip: in Boolean := False);
   --<summary>Computes the HitPoint WorldHp & WorldNv of a ShadePoint</summary>
   --<description>Sets the HitPoint WorldHp & WorldNv of a ShadePoint</description>
   --<parameter name="Sp_Ptr">Pointer to a ShadePoint</parameter>
   --<exception>None at this moment</exception>

   function Get_Object (Sp_Ptr : in ShadePoint_Ptr) return Object_Ptr;
   --<summary>Returns the Object of a ShadePoint</summary>
   --<description>Function to return the Object of a ShadePoint</description>
   --<parameter name="Sp_Ptr">Pointer to a ShadePoint</parameter>
   --<exception>None at this moment</exception>

   procedure Add_ShadePoint (Sp_Ptr : in ShadePoint_Ptr; Sp_List : in out ShadePoint_List);
   --<summary>Adds a ShadePoint to a ShadePoint_List</summary>
   --<description>Adds a ShadePoint to a ShadePoint_List</description>
   --<parameter name="Sp_Ptr">Pointer to the ShadePoint to be added to the list</parameter>
   --<parameter name="Sp_List">The list to add the ShadePoint to</parameter>
   --<exception>None at this moment</exception>

   function Get_No_Of_ShadePoints (Sp_List : in ShadePoint_List) return Large_Integer;
   --<summary>Returns the number of ShadePoint in a ShadePoint_List</summary>
   --<description>Function to return the number of ShadePoint in a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the number of ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   function Get_First_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr;
   --<summary>Returns the first ShadePoint in a ShadePoint_List</summary>
   --<description>Returns the first ShadePoint in a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the first ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr;
   --<summary>Returns the next ShadePoint in a ShadePoint_List</summary>
   --<description>Returns the next ShadePoint in a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the next ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   function Get_Last_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr;
   --<summary>Returns the last ShadePoint in a ShadePoint_List</summary>
   --<description>Returns the last ShadePoint in a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the first ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   function Get_Previous_ShadePoint (Sp_List : in out ShadePoint_List) return ShadePoint_Ptr;
   --<summary>Returns the previous ShadePoint in a ShadePoint_List</summary>
   --<description>Returns the previous ShadePoint in a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the next ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   function Intersect_Objects_With_Ray (R : in Ray; Obj_List, CSG_Obj_List : in out Object_List) return ShadePoint_List;
   --<summary>Casts a ray through the scene and returns the number of hitpoints pushed on the stack</summary>
   --<description>Casts a ray through the scene and returns the number of hitpoints pushed on the stack</description>
   --<parameter name="R">The ray to cast</parameter>
   --<parameter name="Obj_List">The list of non-CSG objects to cast the ray through</parameter>
   --<parameter name="CSG_Obj_List">The list of CSG objects to cast the ray through</parameter>
   --<exception>None at this moment</exception>

   function Hit_Objects_With_Ray (R : in Ray; Obj_List, CSG_Obj_List : in out Object_List) return ShadePoint_List;
   --<summary>Casts a shadow ray through the scene and returns the number of hitpoints pushed on the stack</summary>
   --<description>Casts a shadow ray through the scene and returns the number of hitpoints pushed on the stack</description>
   --<parameter name="R">The shadow ray to cast</parameter>
   --<parameter name="Obj_List">The list of non-CSG objects to cast the ray through</parameter>
   --<parameter name="CSG_Obj_List">The list of CSG objects to cast the ray through</parameter>
   --<exception>None at this moment</exception>

   procedure Free_ShadePoint_List (Sp_List : in out ShadePoint_List);
   --<summary>Frees up the memory associated with a ShadePoint_List</summary>
   --<description>Procedure to free up the memory associated with a ShadePoint_List</description>
   --<parameter name="Sp_List">The list to return the number of ShadePoint from</parameter>
   --<exception>None at this moment</exception>

   procedure Check_Sort (Sp_List : in ShadePoint_List);

private

   type ShadePoint is record
      Hp : HitPoint;
      --  The hitpoint for this object
      Obj_Ptr : Object_Ptr;
      --  The object that is hit;
   end record;

   type ShadePoint_List is record
      First_Ptr : ShadePoint_List_Element_Ptr := null;
      --  Pointer to the first element
      Last_Ptr : ShadePoint_List_Element_Ptr := null;
      --  Pointer to the last element
      No_Of_ShadePoints : Large_Integer := 0;
      --  Static counter of elements in this list
      Current_Ptr : ShadePoint_List_Element_Ptr := null;
      --  Pointer to current element, needed when processing through the list
   end record;

   type ShadePoint_List_Element is record
      Sp_Ptr : ShadePoint_Ptr := null;
      --  Pointer to the hitpoint
      Next_Ptr : ShadePoint_List_Element_Ptr := null;
      --  Pointer to the next element
      Prev_Ptr : ShadePoint_List_Element_Ptr := null;
      --  Pointer to the previous element
   end record;

end ShadePoints;

with Core_Types;            use Core_Types;
with Linear_Math;           use Linear_Math;
with Spectra;               use Spectra;
with HitPoints;             use HitPoints;
with Samplers;              use Samplers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The Lights package contains all Abstract Data Types that handle light sources</summary>
--<description>The Lights package contains all Abstract Data Types that handle light sources.
--The actual light source definitions and methods for a particular light source type will be handled in separate child packages</description>
--<group>Lights</group>

package Lights is

   -------------
   --  ADT Light
   -------------

   type Light is tagged private;
   --<summary>ADT of the root type of all lights. It is tagged so it can be used as a Class for dispatching</summary>

   type Light_Ptr is access all Light'Class;
   --<summary>The access type to all Light'Class objects</summary>

   type Light_List is private;
   --<summary>ADT for the root type of all Lists of Objects. It defaults to an empty list, with no elements (Count = 0)</summary>

   type Light_List_Element is private;
   --<summary>ADT of a linked list element of pointers to Light'Class objects</summary>

   type Light_List_Element_Ptr is access all Light_List_Element;
   --<summary>The access type to all Light_List_Element'Class objects</summary>

   procedure Put (Lt : in Light; Msg : in String);
   --<summary>Prints a Light's statistics</summary>
   --<description>Prints a Light's statistics</description>
   --<parameter name="Lt">A light of the Light Class Wide Type</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Lt_List : in out Light_List; Msg : in String := "Light List ");
   --<summary>Prints a Light List</summary>
   --<description>Prints a Light List by traversing the list and printing each light</description>
   --<parameter name="Lt_List">A Light_List</parameter>
   --<exception>None at this moment</exception>

   procedure Add_Light (Lt_Ptr : in Light_Ptr; Lt_List : in out Light_List);
   --<summary>Adds a Light to a Light_List</summary>
   --<description>Adds a Light to a Light_List</description>
   --<parameter name="Lt_Ptr">Pointer to the light to be added to the list</parameter>
   --<parameter name="Lt_List">The list to add the light to</parameter>
   --<exception>None at this moment</exception>

   function Get_No_Of_Lights (Lt_List : in Light_List) return Large_Integer;
   --<summary>Returns the number of lights in a Light_List</summary>
   --<description>Returns the number of lights in a Light_List</description>
   --<parameter name="Lt_List">The list to return the number of lights from</parameter>
   --<exception>None at this moment</exception>

   function Get_First_Light (Lt_List : in out Light_List) return Light_Ptr;
   --<summary>Returns the first light in a Light_List</summary>
   --<description>Function to return the first light in a Light_List</description>
   --<parameter name="Lt_List">The list to return the first light from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Light (Lt_List : in out Light_List) return Light_Ptr;
   --<summary>Returns the next light in a Light_List</summary>
   --<description>Function to return the next light in a Light_List</description>
   --<parameter name="Lt_List">The list to returArrow_Ptr4n the next light from</parameter>
   --<exception>None at this moment</exception>

   function Get_Light_Name (Lt : in Light'Class) return Unbounded_String;
   --<summary>returns the Name of a Light</summary>
   --<description>returns the Name of a Light</description>
   --<parameter name="Lt">A class-wide light</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Light_Name (Lt : in out Light'Class; Name : in Unbounded_String);
   --<summary>Sets the Name of an Light</summary>
   --<description>Sets the Name of an Light</description>
   --<parameter name="Lt">A class-wide Light</parameter>
   --<parameter name="Name">The new Name of the Light</parameter>
   --<exception>None at this moment</exception>

   function Get_Normal_For_Next_Light_Sample (Lt : in Light; Hp : in HitPoint) return Normal_3D;
   --<summary>returns a Normal pointing to the direction of the next light sample for this HitPoint</summary>
   --<description>returns a Normal pointing to the direction of the next light sample for this HitPoint</description>
   --<parameter name="Lt">A class-wide Light</parameter>
   --<parameter name="Hp">The hitpoint to get the normal for</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Light_Sampler (Lt : in out Light; Smp_Ptr : in Sampler_Ptr);
   --<summary>Sets the Light_Sampler of a Light</summary>
   --<description>Sets the Light_Sampler of a Light</description>
   --<parameter name="Lt">A Light</parameter>
   --<parameter name="Smp_Ptr">A pointer to a sampler</parameter>
   --<exception>None at this moment</exception>n>

   function Get_Light_Sampler (Lt : in out Light) return Sampler_Ptr;
   --<summary>Gets the Light_Sampler of a Light</summary>
   --<description>Gets the Light_Sampler of a Light</description>
   --<parameter name="Lt">A Light</parameter>
   --<exception>None at this moment</exception>

   function Incident_Radiance (Lt : in Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum;
   --<summary>Stub function to compute the incoming light for a hitpoint</summary>
   --<description>Stub function to compute the incoming light for a hitpoint</description>
   --<parameter name="Lt">The light, of the Light Class Wide Type</parameter>
   --<parameter name="Hp">Pointer to the hitpoint for which we need to determine the incident radiance coming from this lightsource</parameter>
   --<parameter name="Dir">Normal pointing to the direction where the light is coming from</parameter>
   --<exception>None at this moment</exception>

private

   type Light is tagged record
      Name : Unbounded_String := To_Unbounded_String ("NOT DEFINED");
      --  The Name of the Light
      Light_Sampler_Ptr : Sampler_Ptr := null;
      --  Does this light source cast a shadow?
      Casts_Shadow : Boolean := True;
   end record;

   type Light_List is record
      First_Ptr : Light_List_Element_Ptr := null;
      --  Pointer to the first element
      No_Of_Lights : Large_Integer := 0;
      --  Static counter of elements in this list
      Current_Ptr : Light_List_Element_Ptr := null;
      --  Pointer to current element, needed when processing through the list
   end record;

   type Light_List_Element is record
      Lt_Ptr : Light_Ptr := null;
      --  Pointer to the light
      Next_Ptr : Light_List_Element_Ptr := null;
      --  Pointer to the next element
   end record;

end Lights;

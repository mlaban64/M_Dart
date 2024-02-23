--  <description>
--  Package HUGE_FLOAT_Functions contains the basic functions to handle math
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes;
use StdTypes;
with Ada;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package HUGE_FLOAT_Functions is new Ada.Numerics.Generic_Elementary_Functions (HUGE_FLOAT);

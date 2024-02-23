with Core_Types; use Core_Types;
with Ada.Numerics.Generic_Elementary_Functions;

--<summary>The Small_Float_Functions package inherits all basic math functions to the Small_Float type</summary>
--<description>The Small_Float_Functions package inherits all basic math functions to the Small_Float type</description>
--<group>Core</group>
package Small_Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Small_Float);

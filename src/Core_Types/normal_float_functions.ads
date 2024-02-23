with Core_Types; use Core_Types;
with Ada.Numerics.Generic_Elementary_Functions;

--<summary>The Normal_Float_Functions package inherits all basic math functions to the Normal_Float type</summary>
--<description>The Normal_Float_Functions package inherits all basic math functions to the Normal_Float type</description>
--<group>Core</group>
package Normal_Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Normal_Float);

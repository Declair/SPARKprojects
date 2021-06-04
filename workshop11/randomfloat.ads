with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

package RandomFloat is
   -- Generate a random number from a normal distribution of mean Mu
   --  and standard deviation Sigma
   function NormalDistribution(Mu : Float;
                               Sigma : Float) return Float;

   function RoughNormalDistribution(Mu    : Float;
                                    Sigma : Float) return Float;

   function UniformDistribution(Lower, Upper : Float) return Float;
end RandomFloat;

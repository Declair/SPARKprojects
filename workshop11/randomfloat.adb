with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

package body RandomFloat is

   -- a random number generator
   Seed  : Generator;

   -- Taken from: http://rosettacode.org/wiki/Random_numbers#Ada and
   -- corrected to use ln instead of log base 10
   function NormalDistribution(Mu    : Float;
                               Sigma : Float) return Float is
      Result : Float;
   begin
      Result := Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed))) * Cos (2.0 * Pi * Random (Seed)));
      return Result;
   end NormalDistribution;

   function RoughNormalDistribution(Mu    : Float;
                                    Sigma : Float) return Float is
      Rand1, Rand2, Rand3, Result : Float;
   begin
      Rand1 := (Random(Seed) * 2.0) - 1.0;
      Rand2 := (Random(Seed) * 2.0) - 1.0;
      Rand3 := (Random(Seed) * 2.0) - 1.0;
      Result := Mu + (Sigma * Rand1 + Rand2 + Rand3);
      return Result;
   end RoughNormalDistribution;

   function UniformDistribution(Lower, Upper : Float)
                               return Float is
      Base : Uniformly_Distributed;
      Result : Float;
   begin
      Base := Random(Seed);
      Result := Lower + (Upper - Lower) * Base;
      return Result;
   end UniformDistribution;
end RandomFloat;

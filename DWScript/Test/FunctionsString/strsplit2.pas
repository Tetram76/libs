var b := 'test 1'#13'test 2';

for var e in b.Split(#13) do
   println('-'+e+'-');

b := 'test 3'#13;

for var e in b.Split(#13) do
   println('-'+e+'-');

b := #13'test 4';

for var e in b.Split(#13) do
   println('-'+e+'-');
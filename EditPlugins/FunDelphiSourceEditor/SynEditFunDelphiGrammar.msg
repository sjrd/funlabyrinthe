TSynFunDelphiSyn

tk
Sensitive
IdentStart '_', 'a'..'z', 'A'..'Z':: '_', '0'..'9', 'a'..'z', 'A'..'Z'::

TOKENTYPES
Identifier
Comment Style=[fsItalic]|Foreground=clGreen
Space
String Foreground=clBlue
Key Style=[fsBold]|Foreground=clNavy
|><|

KEYS
unit
uses
const
resourcestring
var
out

procedure
function
private
public
forward

plugin
object
field
effect
tool
obstacle
poscomponent
vehicle
creator
class
components
attributes
actions
messages

message
name
hint
category
zindex
image
action

div
mod
shl
shr
or
and
xor
not
in
is
as

begin
end

string
nil

if
then
else
while
do
repeat
until
for
to
downto
case
of
try
except
on
finally
raise
inherited
with

can
cannot
has
at
least
most
more
less
than
exactly
receives
discards
|><|

CHARS

'A'..'Z', 'a'..'z', '_':: Ident
BeginProc
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
EndProc

|><|

ENCLOSEDBY

Comment,BraceComment,{,},MultiLine
Comment,LineComment,//,
String,String,','

|><|

SAMPLESOURCE
unit Sample;

uses
  Generics, FLBCommon;

// Objet qui permet d'ouvrir un bloc en or ou en argent
object TSpecialKey
  image 'SpecialKey';
  
  action OpenSilverBlock then
    Player discards 1 Self;

  action OpenGoldenBlock then
    Player discards 1 Self;
end;

end.
|><|

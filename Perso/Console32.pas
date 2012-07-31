unit Console32;

interface

type
  COULEUR = (noir, bleu, vert, cyan, rouge, magenta, brun, gris_clair,
    gris_fonce, bleu_clair, vert_clair, cyan_clair, rouge_clair,
    magenta_clair, jaune, blanc, idem);
  REPONSE = (non, oui);
  PLAGE_X = 1..80;
  PLAGE_Y = 1..25;
  RETKEY = record
    AsciiChar: char;
    ScanCode: byte;
  end;

  { sauve la position du curseur}
procedure Sauve_curseur;

{restaure la position du curseur}
procedure Restaure_curseur;

{ positionne le curseur en x,y}
procedure Bouge_Curseur_en_XY(const x: PLAGE_X; const y: PLAGE_Y);

{ positionne le curseur en x}
procedure Bouge_Curseur_en_X(x: Plage_x);

{ positionne le curseur en y}
procedure Bouge_Curseur_en_Y(y: Plage_y);

{ affiche mess en positions x,y de l'écran}
procedure Affiche(const x: PLAGE_X; const y: PLAGE_Y; const mess: string); overload;
procedure Affiche(const x: PLAGE_X; const y: PLAGE_Y; const mess: char); overload;

{fait apparaitre ou disparaitre le curseur}
procedure Curseur_Visible(const rep: REPONSE);

{ indique la position x du curseur}
function PositionX_Curseur: PLAGE_X;

{ indique la position y du curseur}
function PositionY_Curseur: PLAGE_Y;

{ met l'écran dans les couleurs texte et fond d'écran}
procedure Couleur_Ecran(const text, fond: COULEUR);

{ sauve les couleurs texte et fond d'écran}
procedure Sauve_Couleur;

{ restaure les couleurs texte et fond d'écran}
procedure Restaure_Couleur;

{ efface l'écran}
procedure Efface_Ecran;

{présente un  écran  vierge en couleur de fond et text}
procedure Nouvel_Ecran(const text, fond: COULEUR);

{ efface la fin de ligne à partir du curseur}
procedure Efface_Fin_Ligne;

{ efface la fin de page à partir du curseur}
procedure Efface_Fin_Page;

{ sauve le contenu de l'écran texte et couleurs à 10 niveaux}
procedure Sauve_Ecran;

{ restaure le dernier écran}
procedure Restaure_Ecran;

{ saisie d'un caractère sans l'afficher à l'écran
 la fonction retourne RETKEY (AsciiChar+Scancode , utile pour les touches fonction).
 Nono40 a fortement contribué à l'élaboration de cette fonction}
function LireKey: RETKEY;

{ Fonction donnant TRUE si une touche est en attente
 elle ne prend en compte que les touches donnant un caractère, donc
 elle ne fonctionne pas avec les touches de fonctions.
 Nono40 a élaboré cette fonction}
function KeyAppuyee: Boolean;

{ insère une ligne à l'endroit du curseur}
procedure Insere_Ligne;

{ supprime une ligne à l'endroit du curseur}
procedure Supprime_Ligne;

{ temporise tant qu'une touche n'est pas appuyée}
procedure Appuie;

{ met un libellé dans la barre de titre}
procedure Titre_Fenetre(const Titre: string);

{ transforme une chaine de caractères ANSI (windows) en OEM (DOS)}
function Oem(const st: string): string;

{ retourne l'OS utilisé - 0 : Win3.11, 1 : Win9X, 2 : Win2k ou XP}
function VersionWindows: integer;

{ les 3 fonctions infra gèrent la capture d'écran}
procedure Fichier_Ecran_Ouverture(const nomfic: string);
procedure Fichier_Ecran_Capture(const debutligne, finligne: PLAGE_Y);
procedure Fichier_Ecran_Fermeture; {lance automatiquement Wordpad}

function C_Numero_Serie: string;

implementation

uses
  Windows, SysUtils;

type
  ScrBuff = array[0..24, 0..79] of TCharInfo;
  Rbuff = record
    bf: ^Scrbuff;
    sauvecouleurs: byte;
  end;
var
  ecran_handle, clavier_handle: Thandle;
  curseur_info: TConsoleCursorInfo;
  ecran_info: TConsoleScreenBufferInfo;
  PtScrBuff: array[1..10] of Rbuff;
  iPtScrBuff: byte;
  ok: boolean;
  couleurs_sauvees: byte;
  Fill_char: Char;
  NbCharEcrits, NbCharlus: dword;
  SmallRect: TSmallRect;
  Le_curseur_sauve: TCoord;
  fichier_ecran_handle, fichier_ecran_cpt: longint;
  fichier_ecran_nom: string;

function CoordXY(const x, y: byte): TCoord;
var
  c: TCoord;
begin
  c.x := x;
  c.y := y;
  result := c;
end;

procedure Sauve_Curseur;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  Le_curseur_sauve := ecran_info.dwCursorPosition;
end;

procedure Restaure_Curseur;
begin
  ok := SetConsoleCursorPosition(ecran_handle, Le_curseur_sauve);
end;

procedure Bouge_Curseur_en_XY(const x: PLAGE_X; const y: PLAGE_Y);
begin
  ok := SetConsoleCursorPosition(ecran_handle, CoordXY(x - 1, y - 1));
end;

procedure Bouge_Curseur_en_X(x: Plage_x);
var
  y: byte;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  y := ecran_info.dwCursorPosition.Y;
  ok := SetConsoleCursorPosition(ecran_handle, CoordXY(x - 1, y));
end;

procedure Bouge_Curseur_en_Y(y: Plage_y);
var
  x: byte;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  x := ecran_info.dwCursorPosition.X;
  ok := SetConsoleCursorPosition(ecran_handle, CoordXY(x, y - 1));
end;

procedure Affiche(const x: PLAGE_X; const y: PLAGE_Y; const mess: string); overload;
begin
  Bouge_Curseur_en_XY(x, y);
  write(mess);
end;

procedure Affiche(const x: PLAGE_X; const y: PLAGE_Y; const mess: char); overload;
begin
  Bouge_Curseur_en_XY(x, y);
  write(mess);
end;

procedure Curseur_Visible(const rep: REPONSE);
begin
  curseur_info.dwSize := 10;
  curseur_info.bvisible := rep = oui;
  ok := SetConsoleCursorInfo(ecran_handle, curseur_info);
end;

function PositionX_Curseur: PLAGE_X;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  result := ecran_info.dwCursorPosition.X + 1;
end;

function PositionY_Curseur: PLAGE_Y;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  result := ecran_info.dwCursorPosition.y + 1;
end;

procedure Couleur_Ecran(const text, fond: COULEUR);
var
  tx, fd: byte;
  wattr: word;
begin
  ok := GetConsoleScreenBufferInfo(Ecran_handle, ecran_info);
  wattr := ecran_info.wAttributes;
  if text = idem then
    tx := wattr mod 16
  else
    tx := ord(text);
  if fond = idem then
    fd := wattr mod 16
  else
    fd := ord(fond);
  ok := setConsoleTextAttribute(ecran_handle, fd * 16 + tx);
end;

procedure Sauve_Couleur;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  couleurs_sauvees := ecran_info.wattributes;
end;

procedure Restaure_Couleur;
begin
  ok := setConsoleTextAttribute(ecran_handle, couleurs_sauvees);
end;

procedure Efface_Ecran;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  ok := FillConsoleOutputattribute(ecran_Handle,
    ecran_info.wattributes, 80 * 25, CoordXY(0, 0), NbCharEcrits);
  Fill_Char := ' ';
  ok := FillConsoleOutputCharacter(ecran_Handle,
    Fill_Char, 80 * 25, CoordXY(0, 0), NbCharEcrits);
  Bouge_Curseur_en_XY(1, 1);
end;

procedure Nouvel_Ecran(const text, fond: COULEUR);
begin
  Couleur_Ecran(text, fond);
  Efface_Ecran;
end;

procedure Efface_Fin_Ligne;
var
  x: byte;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  x := ecran_info.dwCursorPosition.x;
  ok := FillConsoleOutputCharacter(ecran_Handle,
    ' ', 80 - x, ecran_info.dwCursorPosition, NbCharEcrits);
end;

procedure Efface_Fin_Page;
var
  x, y: byte;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  x := ecran_info.dwCursorPosition.x;
  y := ecran_info.dwCursorPosition.y;
  ok := FillConsoleOutputCharacter(ecran_Handle,
    ' ', 80 * (24 - y) + 80 - x, ecran_info.dwCursorPosition, NbCharEcrits);
end;

procedure Sauve_Ecran;
begin
  SmallRect.Left := 0;
  SmallRect.top := 0;
  SmallRect.right := 79;
  SmallRect.bottom := 24;
  Sauve_Couleur;
  inc(iPtScrBuff);

  new(PtScrBuff[iPtScrBuff].bf);
  PtScrBuff[iPtScrBuff].sauvecouleurs := couleurs_sauvees;
  ok := ReadConsoleOutput(ecran_handle, PtScrBuff[iPtScrBuff].bf, CoordXY(80, 25),
    CoordXY(0, 0), SmallRect);
end;

procedure Restaure_Ecran;
begin
  SmallRect.Left := 0;
  SmallRect.top := 0;
  SmallRect.right := 79;
  SmallRect.bottom := 24;
  ok := WriteConsoleOutput(ecran_handle, PtScrBuff[iPtScrBuff].bf, CoordXY(80, 25),
    CoordXY(0, 0), SmallRect);
  dispose(PtScrBuff[iPtScrBuff].bf);
  couleurs_sauvees := PtScrBuff[iPtScrBuff].sauvecouleurs;
  Restaure_Couleur;
  dec(iPtScrBuff);
end;

function LireKey: RETKEY;
var
  Zone: TInputRecord;
  Lus: Cardinal;
  ok: boolean;
  retour: RETKEY;
begin
  with Zone.Event.KeyEvent do
  begin
    repeat
      { Lecture du buffer des évènement}
      ReadConsoleInput(Clavier_handle, Zone, 1, Lus);

      ok := (Lus > 0)
        and (Zone.EventType = 1)
        and not (bKeyDown)
        and ((AsciiChar <> #0) or
        (wvirtualscancode in [1, 71..73, 75, 77, 79..83]));
    until ok;
    retour.scancode := wvirtualscancode;
    { permet d'harmoniser la touche ESC différents suivant OS }
    if wvirtualscancode = 1 then
      retour.AsciiChar := #27
    else
      retour.AsciiChar := AsciiChar;
    result := retour;
  end;
end;

function KeyAppuyee: Boolean;
const
  TailleBuf = 20;
var
  Zone: array[1..TailleBuf] of TInputRecord;
  EnAttente: Cardinal;
  i: Cardinal;
begin
  {Demande de la liste des évènements en attente}
  PeekConsoleInput(Clavier_handle, Zone[1], TailleBuf, EnAttente);
  Result := False;
  i := 1;
  while not Result and (i <= EnAttente) and (i <= TailleBuf) do
  begin
    Result := (Zone[i].EventType = 1) { C'est un évènement clavier}
    and (Zone[i].Event.KeyEvent.bKeyDown) { C'est une touche appuyée }
    and (Zone[i].Event.KeyEvent.AsciiChar <> #0); { ce n'est pas une touche de controle}
    Inc(i);
  end;
  { Si il n'y a pas de touche, le buffer est vidé car windows
   à une limite très basse d'évènements en attente}
  if (EnAttente <> 0) and not Result then ReadConsoleInput(Clavier_handle, Zone[1], EnAttente, i);
end;

procedure Appuie;
begin
  Lirekey;
end;

procedure Supprime_Ligne;
var
  buffchar: PansiChar;
  y: byte;
begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  y := ecran_info.dwCursorPosition.y;
  new(buffchar);
  ok := ReadConsoleOutputCharacter(ecran_handle, buffchar, 80 * (24 - y),
    CoordXY(0, y + 1), NbCharLus);
  ok := WriteConsoleOutputCharacter(ecran_handle, buffchar, 80 * (24 - y),
    CoordXY(0, y), NbCharEcrits);
  ok := FillConsoleOutputCharacter(ecran_Handle, ' ', 80, coordXY(0, 24),
    NbCharEcrits);
  dispose(buffchar);
end;

procedure Insere_Ligne;
var
  buffchar: PansiChar;
  y: byte;

begin
  ok := GetConsoleScreenBufferInfo(ecran_handle, ecran_info);
  y := ecran_info.dwCursorPosition.y;
  new(buffchar);
  if y < 24 then
    ok := ReadConsoleOutputCharacter(ecran_handle, buffchar, 80 * (24 - y),
      coordXY(0, y), NbCharLus);
  ok := FillConsoleOutputCharacter(ecran_Handle, ' ', 80,
    coordXY(0, y), NbCharEcrits);
  if y < 24 then
    ok := WriteConsoleOutputCharacter(ecran_handle, buffchar, 80 * (24 - y),
      coordXY(0, y + 1), NbCharEcrits);
  dispose(buffchar);
end;

function C_Numero_Serie: string;
var
  SerialNum: pdword; a, b: dword;
  Buffer: array[0..255] of char;
begin
  result := '';
  new(SerialNum);
  if GetVolumeInformation('c:\', Buffer, SizeOf(Buffer), SerialNum, a, b, nil, 0) then result := IntToHex(SerialNum^, 0);
end;

procedure Titre_Fenetre(const Titre: string);
begin
  SetConsoleTitle(PChar(Titre));
end;

function Oem(const st: string): string;
var
  TempBuffer: array[0..255] of Char;
begin
  CharToOem(Pchar(st), @TempBuffer);
  result := StrPas(@TempBuffer);
end;

function VersionWindows: integer;
var
  OsVer: TOSVersionInfo;
begin
  OsVer.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
  GetVersionEx(OsVer);
  result := OsVer.dwPlatformId;
end;

procedure Fichier_Ecran_Ouverture(const nomfic: string);
begin
  fichier_ecran_nom := 'c:\' + nomfic + '.ecr';
  fichier_ecran_handle := FileCreate(fichier_ecran_nom);
end;

procedure Fichier_Ecran_Fermeture;
begin
  FileClose(fichier_ecran_handle);
end;

procedure Fichier_Ecran_Capture(const debutligne, finligne: PLAGE_Y);
var
  leScrBuff: ^ScrBuff;
  sr: TSmallRect;
  ecran_enr: array[0..80] of char;
  i, j: byte;
begin
  sr.Left := 0;
  sr.top := 0;
  sr.right := 79;
  sr.bottom := 24;

  new(leScrBuff);
  ok := ReadConsoleOutput(ecran_handle, leScrBuff, CoordXY(80, 25),
    CoordXY(0, 0), sr);
  ecran_enr[80] := chr(10);
  inc(fichier_ecran_cpt);
  for j := debutligne to finligne do
  begin
    for i := 0 to 79 do
      ecran_enr[i] := leScrBuff^[j - 1, i].asciichar;
    FileWrite(fichier_ecran_handle, ecran_enr, 81);
  end;
  for i := 0 to 79 do
    ecran_enr[i] := '-';
  FileWrite(fichier_ecran_handle, ecran_enr, 81);
end;

initialization
  ecran_handle := GetStdHandle(STD_OUTPUT_HANDLE);
  clavier_handle := GetStdHandle(STD_INPUT_HANDLE);
  iPtScrBuff := 0;

end.


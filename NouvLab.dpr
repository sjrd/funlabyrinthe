library NouvLab;

uses
  SysUtils,
  Forms,
  Classes,
  Controls,
  Windows,
  ScUtils,
  NouvLabMain in 'NouvLabMain.pas' {FormParamsLab},
  NouvLabAdvance in 'NouvLabAdvance.pas' {FormParamsLabMax};

function NewLab(Cols, Rows, Etas : integer) : boolean;
var Params    : TFormParamsLab;
    ParamsMax : TFormParamsLabMax;
    I, J : integer;
    Str, Str1, Str2, Str3 : string;
    Etage, Labyrinthe, Actions : TStringList;
    Terrain1, Terrain2, Terrain3 : Char;
    Ceinture : boolean;
label Debut;
begin
  Params := TFormParamsLab.Create(Application);
Debut :
  if Params.ShowModal = mrCancel then
  begin
    Params.Free;
    Result := False;
    exit;
  end;
  if Params.BoutonTypesIdentiques.Checked then
  begin
    Terrain1 := Terrain1ToChar(Params.ComboTerrain1.ItemIndex);
    Terrain2 := Terrain2ToChar(Params.ComboTerrain2.ItemIndex);
    Terrain3 := Terrain3ToChar(Params.ComboTerrain3.ItemIndex);
    Ceinture := Terrain3 <> #0;
    if Terrain2 = #0 then
    begin
      Str := '';
      for I := 1 to 7 do Str := Str+Terrain1;
      Str1 := '';
      for I := 1 to Cols do Str1 := Str1+Str;
      if Ceinture then
      begin
        Str1[1]            := Terrain3;
        Str1[Length(Str1)] := Terrain3;
        Str := '';
        for I := 1 to 7 do Str := Str+Terrain3;
        Str2 := '';
        for I := 1 to Cols do Str2 := Str2+Str;
      end else Str2 := Str1;
      Etage := TStringList.Create;
      Etage.Add(Str2);
      for I := 3 to Rows*7 do Etage.Add(Str1);
      Etage.Add(Str2);
    end else
    begin
      Str := '';
      for I := 1 to 7 do Str := Str+Terrain1;
      Str1 := '';
      for I := 1 to Cols do Str1 := Str1+Str;
      Str := '';
      for I := 1 to 7 do
        if (I mod 2) = 0 then Str := Str+Terrain1
                         else Str := Str+Terrain2;
      Str3 := '';
      for I := 1 to Cols do Str3 := Str3+Str;
      if Ceinture then
      begin
        Str1[1]            := Terrain3;
        Str1[Length(Str1)] := Terrain3;
        Str3[1]            := Terrain3;
        Str3[Length(Str3)] := Terrain3;
        Str := '';
        for I := 1 to 7 do Str := Str+Terrain3;
        Str2 := '';
        for I := 1 to Cols do Str2 := Str2+Str;
      end else Str2 := Str3;
      Etage := TStringList.Create;
      Etage.Add(Str2);
      for I := 1 to Rows do for J := 1 to 7 do
      begin
        if ((I = 1) and (J = 1)) or ((I = Rows) and (J = 7)) then Continue;
        if (J mod 2) = 0 then Etage.Add(Str1) else Etage.Add(Str3);
      end;
      Etage.Add(Str2);
    end;
  end else
  begin
    ParamsMax := TFormParamsLabMax.Create(Application);
    if not ParamsMax.QueryParams(Cols, Rows) then
    begin
      ParamsMax.Free;
      goto Debut;
    end;
    Etage := TStringList.Create;
    Etage.Assign(ParamsMax.Etage);
    ParamsMax.Free;
  end;
  Result := True;
  Str := '';
  for I := 1 to Cols*7 do Str := Str+'B';
  Labyrinthe := TStringList.Create;
  Labyrinthe.Add('[Labyrinthe]');
  for I := 0 to Etage.Count-1 do Labyrinthe.Add(Etage[I]);
  for J := 2 to Etas do
  begin
    for I := 1 to 7 do Labyrinthe.Add(Str);
    for I := 0 to Etage.Count-1 do Labyrinthe.Add(Etage[I]);
  end;
  Actions := TStringList.Create;
  Actions.Clear;
  if Params.ComboBord.ItemIndex > 0 then
  begin
    Actions.Add('[Debut]');
    Actions.Add('Remplacer Bord '+Terrain4ToChar(Params.ComboBord.ItemIndex));
  end;
  Actions.Add('[FinDesActions]');
  Params.Free;
  Labyrinthe.SaveToFile(Dir+'Labyrinthe');
  Actions   .SaveToFile(Dir+'Actions'   );
  Labyrinthe.Free;
  Actions.Free;
end;

exports NewLab name 'Nouveau';

begin
end.

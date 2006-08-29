unit ButtonActions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, LabyrintheUtils, Buttons, ScLists,
  ScStrUtils, ScUtils, StrUtils;

type
  TFormModifieBoutons = class(TForm)
    Carte: TImage;
    Horizontal: TScrollBar;
    Vertical: TScrollBar;
    BoutonsEtage: TSpinButton;
    EditNo: TSpinEdit;
    LabelNo: TLabel;
    LabelEtage: TLabel;
    EditEtage: TSpinEdit;
    CasesRemps: TListView;
    BoutonAjouter: TButton;
    BoutonSupprimer: TButton;
    LabelMessage: TLabel;
    EditMessage: TMemo;
    CheckDesactiver: TCheckBox;
    GroupMessage: TRadioGroup;
    CheckUnique: TCheckBox;
    LabelRemplacement: TLabel;
    ComboStyle: TComboBox;
    LabelStyle: TLabel;
    Sep1: TBevel;
    Sep3: TBevel;
    Sep4: TBevel;
    Sep5: TBevel;
    Sep6: TBevel;
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
    LabelSon: TLabel;
    CheckUniqueSon: TCheckBox;
    LabelBord: TLabel;
    ComboBord: TComboBox;
    LabelNomBouton: TLabel;
    EditNomBouton: TComboBox;
    ComboSon: TComboBox;
    LabelCouleur: TLabel;
    ComboCouleur: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure HorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure CarteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditNoChange(Sender: TObject);
    procedure BoutonsEtageDownClick(Sender: TObject);
    procedure BoutonsEtageUpClick(Sender: TObject);
    procedure EditEtageChange(Sender: TObject);
    procedure BoutonAjouterClick(Sender: TObject);
    procedure BoutonSupprimerClick(Sender: TObject);
    procedure SaveMessage(Sender: TObject);
    procedure CheckDesactiverClick(Sender: TObject);
    procedure GroupMessageClick(Sender: TObject);
    procedure ComboStyleChange(Sender: TObject);
    procedure EditNomBoutonChange(Sender: TObject);
    procedure ComboSonChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBordChange(Sender: TObject);
    procedure ComboCouleurChange(Sender: TObject);
  private
    { Déclarations privées }
    ImagesCases : TImageList;
    CurrentNo, CaseX, CaseY, CaseZ, SelectX, SelectY : integer;
    Actions : array [0..45] of TScStrings;
    StylesBoutons : T45_StyleBouton;
    NomsBoutons : array[1..45] of string;
    CurrentAction : string;
    TypeInfo : integer;
    function GetEcran(X, Y : integer) : integer;
    procedure SaveAction;
  public
    { Déclarations publiques }
    procedure EditBoutons;
    procedure Affiche;
    property Ecran[X, Y : integer] : integer read GetEcran;
  end;

var
  FormModifieBoutons: TFormModifieBoutons;

procedure ContourRectangle(Canvas : TCanvas; X1, Y1, X2, Y2 : integer);

implementation

uses
  EditLabyrintheMain, ReplacementScrewDialog;

{$R *.DFM}

procedure ContourRectangle(Canvas : TCanvas; X1, Y1, X2, Y2 : integer);
begin
  with Canvas do
  begin
    MoveTo(X1, Y1);
    LineTo(X2, Y1);
    LineTo(X2, Y2);
    LineTo(X1, Y2);
    LineTo(X1, Y1);
  end;
end;

procedure TFormModifieBoutons.EditBoutons;
var I : integer;
    Str : string;
    OK : boolean;
    var SR : TSearchRec;
begin
  CaseX := LaCase.X;
  CaseY := LaCase.Y;
  CaseZ := LaCase.Z;
  Horizontal.Max := FormPrincipale.Horizontal.Max;
  Vertical.Max   := FormPrincipale.Vertical.Max;
  Horizontal.Enabled := FormPrincipale.Horizontal.Enabled;
  Vertical.Enabled   := FormPrincipale.Vertical.Enabled;
  Horizontal.Position := CaseX;
  Vertical.Position   := CaseY;
  EditEtage.MaxValue := FormPrincipale.EditEtage.MaxValue;
  EditEtage.Enabled  := FormPrincipale.EditEtage.Enabled;
  EditEtage.Value    := FormPrincipale.EditEtage.Value;
  Actions[0].Assign(Labyrinthe.ActionsDebut.LesActions);
  for I := 1 to 45 do
  begin
    Actions[I].Assign(Labyrinthe.Boutons[I].LesActions);
    StylesBoutons[I] := Labyrinthe.Boutons[I].Style;
    NomsBoutons[I] := Labyrinthe.Boutons[I].Nom;
  end;
  Str := IntToStr(CaseX+SelectX)+' '+IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
  for I := 1 to 45 do
  begin
    if Actions[I].FindText(Str, 0) >= 0 then
    begin
      EditNo.Value := I;
      Break;
    end;
  end;
  with ComboSon.Items do
  begin
    Clear;
    ComboSon.Sorted := True;
    if FindFirst(Dir+'Sons\*.wav', faReadOnly or faArchive, SR) = 0 then
    repeat
      if UpperCase(ExtractFileExt(SR.Name)) <> '.WAV' then Continue;
      OK := True;
      Str := SR.Name;
      for I := 0 to Count-1 do
        if AnsiLowerCase(Strings[I]) = AnsiLowerCase(Str) then
        begin
          OK := False;
          Break;
        end;
      if OK then Add(Str);
    until FindNext(SR) <> 0;
    FindClose(SR);
    if FindFirst(Dir+'Sons\*.mid', faReadOnly or faArchive, SR) = 0 then
    repeat
      if UpperCase(ExtractFileExt(SR.Name)) <> '.MID' then Continue;
      OK := True;
      Str := SR.Name;
      for I := 0 to Count-1 do
        if AnsiLowerCase(Strings[I]) = AnsiLowerCase(Str) then
        begin
          OK := False;
          Break;
        end;
      if OK then Add(Str);
    until FindNext(SR) <> 0;
    FindClose(SR);
    ComboSon.Sorted := False;
    Insert(0, 'Erreur');
    Insert(0, 'Danger');
    Insert(0, 'Question');
    Insert(0, 'Information');
    Insert(0, 'TuutTut');
    Insert(0, '(Aucun son)');
  end;
  with EditNomBouton.Items do
  begin
    Clear;
    if FindFirst(Dir+'Cases\*.bmp', faReadOnly or faArchive, SR) = 0 then
    repeat
      if Pos('cases', LowerCase(SR.Name)) = 1 then Continue;
      Str := SR.Name;
      System.Delete(Str, Length(Str)-3, 4);
      if Str <> '' then Add(Str);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  Affiche;
  if ShowModal = mrCancel then exit;
  for I := 1 to 45 do
  begin
    Labyrinthe.Boutons[I].LesActions := Actions[I];
    Labyrinthe.Boutons[I].Nom := NomsBoutons[I];
    Labyrinthe.Boutons[I].Style := StylesBoutons[I];
  end;
  Labyrinthe.ActionsDebut.LesActions := Actions[0];
  FormPrincipale.Modifie := True;
end;

function TFormModifieBoutons.GetEcran(X, Y : integer) : integer;
begin
  Result := Labyrinthe[CaseX+X, CaseY+Y, CaseZ];
  if Result in Escalier then
  begin
    if Montant(CaseX+X, CaseY+Y, CaseZ)
    then Result := 1
    else Result := 2;
  end;
end;

procedure TFormModifieBoutons.SaveAction;
var I, J : integer;
    Str : string;
begin
  Str := 'Remplacer Case '+IntToStr(CaseX+SelectX)+' '+
         IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
  I := Actions[CurrentNo].FindAtPos(Str);
  if I = -1 then
  begin
    J := 0;
    while J < 10 do
    begin
      Str := 'Deplacer '+Char(J+193)+' Case '+IntToStr(CaseX+SelectX)+' '+
             IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
      I := Actions[CurrentNo].FindAtPos(Str);
      if I <> -1 then Break;
      inc(J);
    end;
  end;
  if I <> -1 then Actions[CurrentNo].Delete(I);
  if (Length(CurrentAction) = 1) and (CurrentAction[1] in ['Á'..'Ê']) then
    Actions[CurrentNo].Add('Deplacer '+CurrentAction+' Case '+IntToStr(CaseX+SelectX)+' '+
                           IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ))
  else if CurrentAction <> '' then
    Actions[CurrentNo].Add('Remplacer Case '+IntToStr(CaseX+SelectX)+' '+
                           IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ)+
                           ' '+CurrentAction);
end;

procedure TFormModifieBoutons.FormCreate(Sender: TObject);
var I : integer;
begin
  ImagesCases := TImageList.CreateSize(30, 30);
  CasesRemps.SmallImages := ImagesCases;
  for I := 0 to 45 do Actions[I] := TScStrings.Create;
  CurrentNo := 1;
  Carte.Picture.Assign(FormPrincipale.Image.Picture);
  SelectX := 3;
  SelectY := 3;
end;

procedure TFormModifieBoutons.HorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp, scLineDown, scEndScroll : ;
    scPageUp :
    begin
      ScrollPos := CaseX - CaseX mod 7;
      if ScrollPos = CaseX then dec(ScrollPos, 7);
    end;
    scPageDown : ScrollPos := CaseX - CaseX mod 7 + 7;
    else exit;
  end;
  CaseX := ScrollPos;
  Affiche;
end;

procedure TFormModifieBoutons.VerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp, scLineDown, scEndScroll : ;
    scPageUp :
    begin
      ScrollPos := CaseY - CaseY mod 7;
      if ScrollPos = CaseY then dec(ScrollPos, 7);
    end;
    scPageDown : ScrollPos := CaseY - CaseY mod 7 + 7;
    else exit;
  end;
  CaseY := ScrollPos;
  Affiche;
end;

procedure TFormModifieBoutons.Affiche;
var I, J, L, M, N, X, Y : integer;
    Points : array [0..3] of TPoint;
    B : TBitmap;
    Str, Str2 : string;
    OK : boolean;
    StBouton : TStyleBouton;
label
  ApresMessage;
begin
  with Carte.Picture.Bitmap do
  begin
    with Canvas do
    begin
      for X := 0 to 6 do for Y := 0 to 6 do
      begin
        if (Ecran[X,Y] in Bouton) then StBouton := StylesBoutons[CodeToNoBouton(Ecran[X,Y])]
                                  else StBouton := Labyrinthe.StyleBouton(Ecran[X,Y]);
        if (StBouton in StylesPerso) then
          CopyRect(GetRect(X, Y), Labyrinthe.CodeToImage(Ecran[X, Y]).Canvas, Rect(0, 0, 30, 30)) else
          CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(Ecran[X, Y], StBouton));
        // attention ! l'image n'est pas adaptée si c'est un bouton perso !   
        if Ecran[X, Y] = Depart then
        begin
          Brush.Color := clBlue;
          Pen.Color := clBlue;
          Ellipse(X*30+6, Y*30+6, X*30+24, Y*30+24);
        end;
        if Ecran[X, Y] in Bouton then
        begin
          Brush.Color := clWhite;
          Font.Color := clBlack;
          Font.Size := 8;
          Font.Name := 'Arial';
          N := Ecran[X, Y];
          if (N < 128) then dec(N, 32) else dec(N, 145);
          TextOut(X*30+9, Y*30+9, IntToStr0(N, 2));
        end;
        if Ecran[X, Y] in Teleporteur then
        begin
          Brush.Color := clWhite;
          Font.Color := clRed;
          Font.Size := 8;
          Font.Name := 'Arial';
          TextOut(X*30+9, Y*30+9, IntToStr0(Ecran[X, Y]-96, 2));
        end;
        if Ecran[X, Y] in Barque then
        begin
          Brush.Color := clWhite;
          Font.Color := clNavy;
          Font.Size := 8;
          Font.Name := 'Arial';
          TextOut(X*30+9, Y*30+9, IntToStr0(Ecran[X, Y]-192, 2));
        end;
        if Ecran[X, Y] = FauxMur then
        begin
          Brush.Color := clWhite;
          Font.Color := clBlack;
          Font.Size := 12;
          Font.Style := [fsBold];
          Font.Name := 'Courier';
          TextOut(X*30+10, Y*30+8, '!');
          Font.Style := [];
        end;
      end;
      Brush.Color := clBlue;
      Pen.Color := clBlue;
    end;
    for X := 0 to 6 do for Y := 0 to 6 do if (X <> SelectX) or (Y <> SelectY) then
    begin
      Str := 'Remplacer Case '+IntToStr(CaseX+X)+' '+IntToStr(CaseY+Y)+' '+IntToStr(CaseZ);
      if Actions[CurrentNo].FindText(Str, 0) <> -1 then
      begin
        ContourRectangle(Canvas, X*30-1, Y*30-1, X*30+31, Y*30+31);
        ContourRectangle(Canvas, X*30  , Y*30  , X*30+30, Y*30+30);
        ContourRectangle(Canvas, X*30+1, Y*30+1, X*30+29, Y*30+29);
      end else for N := 193 to 202 do
      begin
        Str := 'Deplacer '+Char(N)+' Case '+IntToStr(CaseX+X)+
               ' '+IntToStr(CaseY+Y)+' '+IntToStr(CaseZ);
        if Actions[CurrentNo].FindText(Str, 0) <> -1 then
        begin
          ContourRectangle(Canvas, X*30-1, Y*30-1, X*30+31, Y*30+31);
          ContourRectangle(Canvas, X*30  , Y*30  , X*30+30, Y*30+30);
          ContourRectangle(Canvas, X*30+1, Y*30+1, X*30+29, Y*30+29);
          Break;
        end;
      end;
    end;
    with Canvas do
    begin
      Brush.Color := clYellow;
      Pen.Color := clYellow;
    end;
    X := SelectX;
    Y := SelectY;
    ContourRectangle(Canvas, X*30-1, Y*30-1, X*30+31, Y*30+31);
    ContourRectangle(Canvas, X*30  , Y*30  , X*30+30, Y*30+30);
    ContourRectangle(Canvas, X*30+1, Y*30+1, X*30+29, Y*30+29);
  end;
  EditEtage.Value := CaseZ;
  Str := 'Remplacer Case '+IntToStr(CaseX+SelectX)+' '+
         IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
  I := Actions[CurrentNo].FindAtPos(Str);
  if I = -1 then
  begin
    J := 0;
    while J < 10 do
    begin
      Str := 'Deplacer '+Char(J+193)+' Case '+IntToStr(CaseX+SelectX)+' '+
             IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
      if Actions[CurrentNo].FindAtPos(Str) <> -1 then
      begin
        CurrentAction := Char(J+193);
        Break;
      end;
      inc(J);
    end;
    if J = 10 then CurrentAction := '';
  end else CurrentAction := GetLastToken(Actions[CurrentNo][I], ' ');
  ImagesCases.Clear;
  CasesRemps.Items.Clear;
  I := 1;
  B := TBitmap.Create;
  B.Height := 30;
  B.Width := 30;
  while I <= Length(CurrentAction) do
  begin
    J := Byte(CurrentAction[I]);
    if (J in Bouton) then StBouton := StylesBoutons[CodeToNoBouton(J)]
                     else StBouton := Labyrinthe.StyleBouton(J);
    if (StBouton in StylesPerso) then
      B.Canvas.CopyRect(Rect(0, 0, 30, 30), Labyrinthe.CodeToImage(J).Canvas, Rect(0, 0, 30, 30)) else
      B.Canvas.CopyRect(Rect(0, 0, 30, 30), Cases.Canvas, CaseRect(J, StBouton));
    // attention ! l'image n'est pas adaptée si c'est un bouton perso !   
    if J = FauxMur then with B.Canvas do
    begin
      Brush.Color := clWhite;
      Font.Color := clBlack;
      Font.Size := 12;
      Font.Style := [fsBold];
      Font.Name := 'Courier';
      TextOut(10, 8, '!');
      Font.Style := [];
    end;
    ImagesCases.Add(B, nil);
    with CasesRemps.Items.Add do
    begin
      Caption := CaseToStr(J);
      ImageIndex := I-1;
    end;
    inc(I);
  end;
  EditMessage.OnExit  := nil;
  CheckUnique.OnClick := nil;
  EditMessage.Enabled  := True;
  GroupMessage.Enabled := True;
  CheckUnique.Enabled  := CurrentNo > 0;
  EditMessage.Lines.Clear;
  L := 0;
  M := 0;
  N := 0;
  I := -1;
  while I < Actions[CurrentNo].Count do
  begin
    I := Actions[CurrentNo].FindText('Message', I+1);
    if (I = -1) then break else begin L := 0; M := I; inc(N) end;
  end;
  I := -1;
  while I < Actions[CurrentNo].Count do
  begin
    I := Actions[CurrentNo].FindText('Indice', I+1);
    if (I = -1) then break else begin L := 1; M := I; inc(N) end;
  end;
  I := -1;
  while I < Actions[CurrentNo].Count do
  begin
    I := Actions[CurrentNo].FindText('Impasse', I+1);
    if (I = -1) then break else begin L := 2; M := I; inc(N) end;
  end;
  TypeInfo := L;
  if (N = 0) then
  begin
    GroupMessage.ItemIndex := 0;
    CheckUnique.Checked := False;
    goto ApresMessage;
  end;
  if (N > 1) then
  begin
    EditMessage.Text := 'Les informations qu''affichent ce bouton '+
                        'ont été créées avec l''éditeur d''actions.'+#13#10+
                        'Il est impossible de les modifier avec cet éditeur.';
    GroupMessage.ItemIndex := -1;
    CheckUnique.Checked  := False;
    EditMessage.Enabled  := False;
    GroupMessage.Enabled := False;
    CheckUnique.Enabled  := False;
    goto ApresMessage;
  end;
  GroupMessage.ItemIndex := L;
  Str := Actions[CurrentNo][M];
  case L of
  0 : begin
       Str2 := 'Message';
       OK := ((Copy(Str, 1,  7) = 'Message') or
             (Copy(Str, 1, 14) = 'Unique Message'));
      end;
  1 : begin
       Str2 := 'Indice';
       OK := ((Copy(Str, 1,  6) = 'Indice') or
             (Copy(Str, 1, 13) = 'Unique Indice'));
      end;
  2 : begin
       Str2 := 'Impasse';
       OK := ((Copy(Str, 1,  7) = 'Impasse') or
             (Copy(Str, 1, 14) = 'Unique Impasse'));
      end;
  end; //case
  if not OK then
  begin
    EditMessage.Text := 'L''information qu''affiche ce bouton '+
                      'a été créée avec l''éditeur d''actions.'+#13#10+
                      'Il est impossible de la modifier avec cet éditeur.';
    CheckUnique.Checked  := False;
    EditMessage.Enabled  := False;
    GroupMessage.Enabled := False;
    CheckUnique.Enabled  := False;
    goto ApresMessage;
  end;
  if GetXToken(Str, ' ', 1) = 'Unique' then
  begin
    CheckUnique.Checked := True;
    Str2 := 'Unique '+Str2;
  end else CheckUnique.Checked := False;
  Str := Copy(Str, Length(Str2)+2, Length(Str));
  I := Pos('{', Str);
  J := PosEx('}', Str, I+1);
  if (I > 0) and (J > I) then
  begin
    Str := Copy(Str, I+1, J-I-1);
    while Str <> '' do
    begin
      Str2 := GetFirstToken(Str, '\');
      EditMessage.Lines.Add(Str2);
      Delete(Str, 1, Length(Str2)+1);
    end;
  end;
ApresMessage :
  EditMessage.OnExit   := SaveMessage;
  CheckUnique.OnClick  := SaveMessage;
  CheckDesactiver.OnClick := nil;
  CheckDesactiver.Enabled := CurrentNo > 0;
  CheckDesactiver.Checked := (Actions[CurrentNo].FindAtPos('Desactiver') <> -1) or
                             (Actions[CurrentNo].IndexOf('Remplacer Ici @') <> -1);
  CheckDesactiver.OnClick := CheckDesactiverClick;
  ComboStyle.OnChange := nil;
  ComboStyle.Enabled := CurrentNo > 0;
  ComboStyle.ItemIndex := IIF(CurrentNo = 0, -1,
                              integer(StylesBoutons[CurrentNo]));
  ComboStyle.OnChange := ComboStyleChange;
  EditNomBouton.OnChange := nil;
  EditNomBouton.Enabled := (CurrentNo > 0) and
                           (StylesBoutons[CurrentNo] in StylesPerso);
  if EditNomBouton.Enabled then EditNomBouton.Color := clWindow
                           else EditNomBouton.Color := clBtnFace;
  if CurrentNo = 0 then
    EditNomBouton.Text := ''
  else
    EditNomBouton.Text := NomsBoutons[CurrentNo];
  EditNomBouton.OnChange := EditNomBoutonChange;
  ComboSon.OnChange := nil;
  CheckUniqueSon.OnClick := nil;
  CheckUniqueSon.Enabled := CurrentNo > 0;
  N := Actions[CurrentNo].FindAtPos('Son');
  if N = -1 then N := Actions[CurrentNo].FindAtPos('Unique Son');
  if N = -1 then
  begin
    ComboSon.Text := '(Aucun son)';
    CheckUniqueSon.Checked := False;
  end else
  begin
    Str := Actions[CurrentNo][N];
    I := Pos('{', Str)+1;
    if I = 1 then ComboSon.Text := '(Aucun son)' else
    begin
      J := PosEx('}', Str, I);
      ComboSon.Text := Copy(Str, I, J-I);
    end;
    CheckUniqueSon.Checked := Copy(Str, 1, 6) = 'Unique';
  end;
  CheckUniqueSon.OnClick := ComboSonChange;
  ComboSon.OnChange := ComboSonChange;
  ComboBord.OnChange := nil;
  N := Actions[CurrentNo].FindAtPos('Remplacer Bord');
  if N = -1 then ComboBord.ItemIndex := 0 else
  begin
    Str := Actions[CurrentNo][N];
    Str := GetXToken(Str, ' ', 3);
    if Str = '' then ComboBord.ItemIndex := 0 else
    case Str[1] of
      'B' : ComboBord.ItemIndex := 1;
      '1' : ComboBord.ItemIndex := 2;
      '2' : ComboBord.ItemIndex := 3;
      '3' : ComboBord.ItemIndex := 4;
      '0' : ComboBord.ItemIndex := 5;
      'â' : ComboBord.ItemIndex := 6;
      else  ComboBord.ItemIndex := -1;
    end;
  end;
  ComboBord.OnChange := ComboBordChange;
  ComboCouleur.OnChange := nil;
  N := Actions[CurrentNo].FindAtPos('Remplacer &Couleur');
  if N = -1 then ComboCouleur.ItemIndex := 0 else
  begin
    Str := Actions[CurrentNo][N];
    Str := GetXToken(Str, ' ', 3);
    ComboCouleur.Text := Str;
  end;
  ComboCouleur.OnChange := ComboCouleurChange;
end;

procedure TFormModifieBoutons.CarteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var I : integer;
    Str : string;
begin
  if EditMessage.Enabled then SaveMessage(Sender);
  if Button <> mbLeft then exit;
  SelectX := X div 30;
  SelectY := Y div 30;
  Str := IntToStr(CaseX+SelectX)+' '+IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
  for I := 1 to 45 do
  begin
    if Actions[I].FindText(Str, 0) >= 0 then
    begin
      EditNo.Value := I;
      Break;
    end;
  end;
  Affiche;
end;

procedure TFormModifieBoutons.EditNoChange(Sender: TObject);
begin
  CurrentNo := EditNo.Value;
  Affiche;
end;

procedure TFormModifieBoutons.BoutonsEtageDownClick(Sender: TObject);
begin
  SaveMessage(Sender);
  if CaseZ > 1 then EditEtage.Value := CaseZ-1;
end;

procedure TFormModifieBoutons.BoutonsEtageUpClick(Sender: TObject);
begin
  SaveMessage(Sender);
  if CaseZ < EditEtage.MaxValue then EditEtage.Value := CaseZ+1;
end;

procedure TFormModifieBoutons.EditEtageChange(Sender: TObject);
begin
  CaseZ := EditEtage.Value;
  Affiche;
end;

procedure TFormModifieBoutons.BoutonAjouterClick(Sender: TObject);
var I, J : integer;
    Str : string;
begin
  Str := 'Remplacer Case '+IntToStr(CaseX+SelectX)+' '+IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
  I := Actions[CurrentNo].FindText(Str, 0);
  if I = -1 then for J := 193 to 202 do
  begin
    Str := 'Deplacer '+Char(J)+' Case '+IntToStr(CaseX+SelectX)+
           ' '+IntToStr(CaseY+SelectY)+' '+IntToStr(CaseZ);
    if Actions[CurrentNo].FindText(Str, 0) <> -1 then Break;
  end;
  if (I <> -1) and (CurrentAction = '') then
  begin
    ShowMes('Actions non modifiables', 'Les actions effectuées sur cette case'+#10+
            'ont été créées avec l''éditeur d''actions et'+#10+
            'ne sont pas modifiables avec cet éditeur.', MB_OK or MB_ICONERROR);
    exit;
  end;
  I := FormNewCase.NewCase;
  if I = 0 then exit;
  CurrentAction := CurrentAction + Char(I);
  SaveAction;
  Affiche;
end;

procedure TFormModifieBoutons.BoutonSupprimerClick(Sender: TObject);
begin
  if CasesRemps.Selected <> nil then
  begin
    Delete(CurrentAction, CasesRemps.Selected.Index+1, 1);
    SaveAction;
    Affiche;
  end else
    ShowMes('Erreur', 'Aucune case de remplacement sélectionnée.',
            MB_OK or MB_ICONERROR);
end;

procedure TFormModifieBoutons.SaveMessage(Sender: TObject);
var I : integer;
    Str, Str2 : string;
begin
  case GroupMessage.ItemIndex of
    0 : Str2 := 'Message';
    1 : Str2 := 'Indice';
    2 : Str2 := 'Impasse';
  end;
  I := Actions[CurrentNo].FindText(Str2, 0);
  if I <> -1 then Actions[CurrentNo].Delete(I);
  if EditMessage.Text = '' then exit;
  Str := EditMessage.Lines[0];
  for I := 1 to EditMessage.Lines.Count-1 do
    Str := Str+'\'+EditMessage.Lines[I];
  Str := Str2 + ' {' + Str + '}';
  if CheckUnique.Checked then Str := 'Unique '+Str;
  Actions[CurrentNo].Add(Str);
end;

procedure TFormModifieBoutons.CheckDesactiverClick(Sender: TObject);
var I : integer;
begin
  if CheckDesactiver.Checked then
  begin
    Actions[CurrentNo].Add('Desactiver');
  end else
  begin
    I := Actions[CurrentNo].FindAtPos('Desactiver');
    if I = -1 then I := Actions[CurrentNo].IndexOf('Remplacer Ici @');
    if I <> -1 then Actions[CurrentNo].Delete(I);
  end;
end;

procedure TFormModifieBoutons.GroupMessageClick(Sender: TObject);
var I : integer;
    Str, Str2 : string;
begin
  if EditMessage.Text = '' then exit;
  Str := EditMessage.Lines[0];
  for I := 1 to EditMessage.Lines.Count-1 do
    Str := Str+'\'+EditMessage.Lines[I];
  case TypeInfo of
    0 : Str2 := 'Message';
    1 : Str2 := 'Indice';
    2 : Str2 := 'Impasse';
  end;
  case GroupMessage.ItemIndex of
    0 : Str := 'Message {'+Str+'}';
    1 : Str := 'Indice {'+Str+'}';
    2 : Str := 'Impasse {'+Str+'}';
  end;
  if CheckUnique.Checked then
    Str  := 'Unique '+Str;

  I := Actions[CurrentNo].FindText(Str2, 0);
  if I <> -1 then Actions[CurrentNo][I] := Str
             else Actions[CurrentNo].Add(Str);
  Affiche;
end;

procedure TFormModifieBoutons.ComboStyleChange(Sender: TObject);
begin
  StylesBoutons[CurrentNo] := TStyleBouton(ComboStyle.ItemIndex);
  if ComboStyle.ItemIndex < 4 then NomsBoutons[CurrentNo] := '';
  if ComboStyle.ItemIndex = 5 then CheckDesactiver.Checked := True;
  Affiche;
end;

procedure TFormModifieBoutons.EditNomBoutonChange(Sender: TObject);
begin
  NomsBoutons[CurrentNo] := EditNomBouton.Text;
  Affiche;
end;

procedure TFormModifieBoutons.ComboSonChange(Sender: TObject);
var I : integer;
    Str : string;
begin
  I := Actions[CurrentNo].FindAtPos('Son');
  if I = -1 then I := Actions[CurrentNo].FindAtPos('Unique Son');
  if I = -1 then I := Actions[CurrentNo].Count
            else Actions[CurrentNo].Delete(I);
  if (ComboSon.Text = '(Aucun son)') or (ComboSon.Text = '') then exit;
  Str := 'Son {'+ComboSon.Text+'}';
  if CheckUniqueSon.Checked then Str := 'Unique '+Str;
  Actions[CurrentNo].Insert(I, Str);
  Affiche;
end;

procedure TFormModifieBoutons.FormDestroy(Sender: TObject);
var I : integer;
begin
  for I := 0 to 45 do Actions[I].Free;
  ImagesCases.Free;
end;

procedure TFormModifieBoutons.ComboBordChange(Sender: TObject);
var I : integer;
    C : Char;
begin
  I := Actions[CurrentNo].FindAtPos('Remplacer Bord');
  if I = -1 then I := Actions[CurrentNo].Count
            else Actions[CurrentNo].Delete(I);
  case ComboBord.ItemIndex of
    0 : exit;
    1 : C := 'B';
    2 : C := '1';
    3 : C := '2';
    4 : C := '3';
    5 : C := '0';
    6 : C := 'â';
    else C := #0;
  end;
  Actions[CurrentNo].Insert(I, 'Remplacer Bord '+C);
  Affiche;
end;

procedure TFormModifieBoutons.ComboCouleurChange(Sender: TObject);
var I : integer;
begin
  I := Actions[CurrentNo].FindAtPos('Remplacer &Couleur');
  if I = -1 then I := Actions[CurrentNo].Count
            else Actions[CurrentNo].Delete(I);
  if ComboCouleur.ItemIndex = 0 then exit;
  Actions[CurrentNo].Insert(I, 'Remplacer &Couleur '+ComboCouleur.Text);
  Affiche;
end;

end.

unit SimpleSquaresSquarePosEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FilesUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor, SimpleSquaresEditorPart,
  StdCtrls, Spin, ScUtils, SdDialogs, SimpleSquaresConsts;

type
  TFrameSquarePosEditor = class(TFrameSimpleSquaresEditorPart)
    GroupBoxSquarePos: TGroupBox;
    LabelMap: TLabel;
    LabelAbsolutePosX: TLabel;
    LabelAbsolutePosY: TLabel;
    LabelAbolutePosZ: TLabel;
    ButtonCurrentSquare: TRadioButton;
    ButtonAbsolutePos: TRadioButton;
    ButtonResetSquarePos: TButton;
    EditMapID: TComboBox;
    EditAbsolutePosX: TSpinEdit;
    EditAbsolutePosY: TSpinEdit;
    EditAbsolutePosZ: TSpinEdit;
    procedure ButtonCurrentSquareClick(Sender: TObject);
    procedure ButtonAbsolutePosClick(Sender: TObject);
    procedure ButtonResetSquarePosClick(Sender: TObject);
    procedure EditAbsolutePosEnter(Sender: TObject);
    procedure EditAbsolutePosChange(Sender: TObject);
  private
    MasterFile: TMasterFile;    /// Fichier ma�tre
    MapViewer: IOTAMapViewer50; /// Visualisateur de cartes

    FCurrentSquarePos: TSquarePos; /// Position de case � �diter

    procedure FillControls;

    function GetCaption: string;
    procedure SetCaption(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Activate;
    procedure Deactivate;

    property Caption: string read GetCaption write SetCaption;
  published
    property CurrentSquarePos: TSquarePos
      read FCurrentSquarePos write FCurrentSquarePos;
  end;

implementation

{$R *.dfm}

{*
  [@inheritDoc]
*}
constructor TFrameSquarePosEditor.Create(AOwner: TComponent);
begin
  CustomVisibility := True;

  inherited;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Case courante
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSquarePosEditor.ButtonCurrentSquareClick(
  Sender: TObject);
begin
  EditMapID.Text := '';
  EditAbsolutePosX.Value := 0;
  EditAbsolutePosY.Value := 0;
  EditAbsolutePosZ.Value := 0;

  with CurrentSquarePos do
  begin
    Origin := poCurrent;
    MapID := '';
    Offset := Point3D(0, 0, 0);
  end;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Position absolue
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSquarePosEditor.ButtonAbsolutePosClick(
  Sender: TObject);
begin
  CurrentSquarePos.Origin := poAbsolute;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Res�lectionner la case
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSquarePosEditor.ButtonResetSquarePosClick(
  Sender: TObject);
begin
  if (not MapViewer.Visible) or (MapViewer.SelectedMap = nil) then
  begin
    // Nothing selected
    MapViewer.Visible := True;
    ShowDialog(SNoSquareSelectedTitle, SNoSquareSelected, dtError);
    Exit;
  end;

  CurrentSquarePos.SetToQPos(MapViewer.SelectedSquare);
  FillControls;
  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnEnter des contr�les de position absolue
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSquarePosEditor.EditAbsolutePosEnter(Sender: TObject);
begin
  ButtonAbsolutePos.Checked := True;
end;

{*
  Gestionnaire d'�v�nement OnChange des contr�les de position absolue
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSquarePosEditor.EditAbsolutePosChange(
  Sender: TObject);
begin
  with CurrentSquarePos do
  begin
    MapID := EditMapID.Text;

    Offset := Point3D(EditAbsolutePosX.Value, EditAbsolutePosY.Value,
      EditAbsolutePosZ.Value);
  end;

  MarkModified;
end;

{*
  Renseigne les contr�les qui commande la position de la case � modifier
*}
procedure TFrameSquarePosEditor.FillControls;
begin
  ButtonCurrentSquare.OnClick := nil;
  ButtonAbsolutePos.OnClick := nil;
  EditMapID.OnChange := nil;
  EditAbsolutePosX.OnChange := nil;
  EditAbsolutePosY.OnChange := nil;
  EditAbsolutePosZ.OnChange := nil;

  with CurrentSquarePos do
  begin
    case Origin of
      poCurrent: ButtonCurrentSquare.Checked := True;
      poAbsolute: ButtonAbsolutePos.Checked := True;
    else
      Assert(False);
    end;

    EditMapID.Text := MapID;
    EditAbsolutePosX.Value := Offset.X;
    EditAbsolutePosY.Value := Offset.Y;
    EditAbsolutePosZ.Value := Offset.Z;
  end;

  ButtonCurrentSquare.OnClick := ButtonCurrentSquareClick;
  ButtonAbsolutePos.OnClick := ButtonAbsolutePosClick;
  EditMapID.OnChange := EditAbsolutePosChange;
  EditAbsolutePosX.OnChange := EditAbsolutePosChange;
  EditAbsolutePosY.OnChange := EditAbsolutePosChange;
  EditAbsolutePosZ.OnChange := EditAbsolutePosChange;
end;

{*
  Titre de l'�diteur
  @return Titre de l'�diteur
*}
function TFrameSquarePosEditor.GetCaption: string;
begin
  Result := GroupBoxSquarePos.Caption;
end;

{*
  Modifie le titre de l'�diteur
  @param Value   Nouveau titre
*}
procedure TFrameSquarePosEditor.SetCaption(const Value: string);
begin
  GroupBoxSquarePos.Caption := Value;
end;

{*
  Active l'�diteur
*}
procedure TFrameSquarePosEditor.Activate;
var
  I: Integer;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MapViewer := GetFunLabyEditMainForm.MapViewer;
  end;

  // Activate

  with MasterFile.Master, EditMapID.Items do
  begin
    Clear;
    for I := 0 to MapCount-1 do
      Add(Maps[I].ID);
  end;

  FillControls;
end;

{*
  D�sactive l'�diteur
*}
procedure TFrameSquarePosEditor.Deactivate;
begin
  ButtonCurrentSquare.OnClick := nil;
  ButtonAbsolutePos.OnClick := nil;
  EditMapID.OnChange := nil;
  EditAbsolutePosX.OnChange := nil;
  EditAbsolutePosY.OnChange := nil;
  EditAbsolutePosZ.OnChange := nil;
end;

end.


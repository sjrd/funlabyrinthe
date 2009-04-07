unit SimpleSquaresDeactivateEffectActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions, SimpleSquaresConsts,
  SimpleSquaresActionEditor;

type
  {*
    Cadre d'�dition d'une action D�sactiver un effet
    @author sjrd
    @version 5.0
  *}
  TFrameDeactivateEffectActionEditor = class(TFrameActionEditor)
    LabelEffectID: TLabel;
    SquaresImages: TImageList;
    EditEffectID: TComboBoxEx;
    procedure EditEffectIDChange(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier ma�tre

    FCurrentAction: TDeactivateEffectAction; /// Action courante

    procedure RegisterSingleComponent(Component: TSquareComponent); stdcall;
    procedure RegisterComponentSet(Template: TSquareComponent;
      const Components: array of TSquareComponent; BaseIndex: Integer;
      const DialogTitle, DialogPrompt: string); stdcall;

    procedure FillIDEdits;
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TDeactivateEffectAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{------------------------------------------}
{ TFrameDeactivateEffectActionEditor class }
{------------------------------------------}

{*
  Enregistre un unique composant
  @param Component   Le composant � enregistrer
*}
procedure TFrameDeactivateEffectActionEditor.RegisterSingleComponent(
  Component: TSquareComponent);
var
  SquareBmp: TSquareBitmap;
  ImageIndex: Integer;
  Category: TComboExItems;
  Item: TComboExItem;
begin
  // Choix de la cat�gorie
  if Component is TEffect then
    Category := EditEffectID.ItemsEx
  else
    Exit; // ignore

  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TSquareBitmap.Create;
  try
    Component.Draw(NoQPos, SquareBmp.Canvas);
    ImageIndex := SquaresImages.AddMasked(SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;

  // Ajout du bouton
  Item := Category.Add;
  Item.Caption := Component.ID;
  Item.ImageIndex := ImageIndex;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant mod�le pour l'image et le nom � afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la bo�te de dialogue du choix du num�ro
  @param DialogPrompt   Invite de la bo�te de dialogue du choix du num�ro
*}
procedure TFrameDeactivateEffectActionEditor.RegisterComponentSet(
  Template: TSquareComponent; const Components: array of TSquareComponent;
  BaseIndex: Integer; const DialogTitle, DialogPrompt: string);
begin
  // No support for component sets
end;

{*
  Renseigne les �diteurs ID de composant d'apr�s l'action courante
*}
procedure TFrameDeactivateEffectActionEditor.FillIDEdits;
begin
  with CurrentAction do
  begin
    EditEffectID.Text := IIF(EffectID = '', SNone, EffectID);
    EditEffectID.ItemIndex := EditEffectID.Items.IndexOf(EditEffectID.Text);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDeactivateEffectActionEditor.ActivateAction;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    EditEffectID.ItemsEx.Add.Caption := SNone;

    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MasterFile.RegisterComponents(RegisterSingleComponent,
      RegisterComponentSet);
  end;

  // Activate action

  FillIDEdits;

  EditEffectID.OnChange := EditEffectIDChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDeactivateEffectActionEditor.DeactivateAction;
begin
  EditEffectID.OnChange := nil;
end;

{*
  Gestionnaire d'�v�nement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameDeactivateEffectActionEditor.EditEffectIDChange(
  Sender: TObject);
var
  NewID: TComponentID;
begin
  NewID := (Sender as TComboBoxEx).Text;
  if NewID = SNone then
    NewID := '';

  CurrentAction.EffectID := NewID;

  MarkModified;
end;

end.


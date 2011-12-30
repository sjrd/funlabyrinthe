unit SimpleSquaresSoundActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SimpleSquaresActionEditor, ImgList, StdCtrls, ExtCtrls,
  SimpleSquaresActions, FunLabyUtils, FilesUtils, StrUtils;

type
  {*
    Cadre d'�dition d'une action Jouer un son
    @author sjrd
    @version 5.1
  *}
  TFrameSoundActionEditor = class(TFrameActionEditor)
    LabelSound: TLabel;
    EditSound: TButtonedEdit;
    ButtonImages: TImageList;
    OpenSoundDialog: TOpenDialog;
    procedure EditSoundChange(Sender: TObject);
    procedure EditSoundLeftButtonClick(Sender: TObject);
  private
    FCurrentAction: TSoundAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  public
    procedure AfterConstruction; override;
  published
    property CurrentAction: TSoundAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{-------------------------------}
{ TFrameSoundActionEditor class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.ActivateAction;
begin
  EditSound.Text := CurrentAction.Sound;

  EditSound.OnChange := EditSoundChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.DeactivateAction;
begin
  EditSound.OnChange := nil;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.AfterConstruction;
begin
  inherited;

  OpenSoundDialog.InitialDir :=
    JoinPath([LibraryPath, ResourcesDir, ResourceKindToDir[rkSound]]);
end;

{*
  Gestionnaire d'�v�nement OnChange de la zone de texte Son
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSoundActionEditor.EditSoundChange(Sender: TObject);
begin
  CurrentAction.Sound := EditSound.Text;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnLeftButtonClick de la zone de texte Son
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSoundActionEditor.EditSoundLeftButtonClick(Sender: TObject);
var
  FileName: TFileName;
  MasterFile: TMasterFile;
  HRef: string;
begin
  if OpenSoundDialog.Execute then
  begin
    FileName := OpenSoundDialog.FileName;
    MasterFile := GetFunLabyEditMainForm.MasterFile;
    HRef := MasterFile.MakeResourceHRef(FileName, rkSound);

    EditSound.Text := HRef;

    OpenSoundDialog.InitialDir := ExtractFilePath(FileName);
  end;
end;

end.


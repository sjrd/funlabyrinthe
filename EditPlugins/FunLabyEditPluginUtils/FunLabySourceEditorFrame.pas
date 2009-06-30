unit FunLabySourceEditorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SdDialogs, FilesUtils, SourceEditors;

resourcestring
  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a �t� modifi�. Voulez-vous l''enregistrer ?';

type
  {*
    Classe de base pour les frames qui doivent impl�menter l'interface
    ISourceEditor50 de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TFrameFunLabySourceEditor = class(TFrame, ISourceEditor50)
  private
    FSourceFile: TSourceFile; /// Fichier source
    FModified: Boolean;       /// Indique si le source a �t� modifi�

    FOnStateChange: TSourceEditorNotifyEvent; /// �v�nement OnStateChange
  protected // for support of additionnal interfaces
    function GetSourceFile: TSourceFile;
    function GetControl: TControl;
    function GetModified: Boolean;

    function GetOnStateChange: TSourceEditorNotifyEvent;
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    procedure ISourceEditor50.Release = Free;
  protected
    procedure SetModified(Value: Boolean); virtual;

    procedure DoStateChange; virtual;

    function GetUnitName: string; virtual;

    procedure LoadFile(ASourceFile: TSourceFile); virtual;
    function SaveFile: Boolean; virtual;
    function CanClose: Boolean; virtual;

    property SourceFile: TSourceFile read FSourceFile;
    property Modified: Boolean read FModified write SetModified;

    property OnStateChange: TSourceEditorNotifyEvent
      read FOnStateChange write FOnStateChange;
  end;

implementation

{$R *.dfm}

{----------------------------------}
{ Classe TFrameFunLabySourceEditor }
{----------------------------------}

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetSourceFile: TSourceFile;
begin
  Result := FSourceFile;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetControl: TControl;
begin
  Result := Self;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetModified: Boolean;
begin
  Result := FModified;
end;

{*
  Modifie la propri�t� Modified
  Si la valeur de Modified est chang�e, la m�thode DoStateChange est appel�e
  pour refl�ter le changement d'�tat.
  @param Value   Nouvelle valeur de Modified
*}
procedure TFrameFunLabySourceEditor.SetModified(Value: Boolean);
begin
  if Value <> FModified then
  begin
    FModified := Value;
    DoStateChange;
  end;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetOnStateChange: TSourceEditorNotifyEvent;
begin
  Result := FOnStateChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySourceEditor.SetOnStateChange(
  Value: TSourceEditorNotifyEvent);
begin
  FOnStateChange := Value;
end;

{*
  Notifie un changement de l'�tat
*}
procedure TFrameFunLabySourceEditor.DoStateChange;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

{*
  Nom de l'unit� �dit�e
  @return Nom de l'unit� �dit�e
*}
function TFrameFunLabySourceEditor.GetUnitName: string;
begin
  Result := ChangeFileExt(ExtractFileName(SourceFile.FileName), '');
end;

{*
  Charge un fichier source
  @param ASourceFile   Fichier source � charger
*}
procedure TFrameFunLabySourceEditor.LoadFile(ASourceFile: TSourceFile);
begin
  FSourceFile := ASourceFile;

  FModified := False;
end;

{*
  Enregistre le fichier source
  Appelle toujours DoStateChange.
*}
function TFrameFunLabySourceEditor.SaveFile: Boolean;
begin
  FModified := False;
  DoStateChange;

  Result := True;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.CanClose: Boolean;
begin
  if not Modified then
    Result := True
  else
  begin
    case ShowDialog(sConfirmExitTitle, sConfirmExit, dtConfirmation,
      dbYesNoCancel) of
      drYes: Result := SaveFile;
      drNo: Result := True;
    else
      Result := False;
    end;
  end;
end;

end.

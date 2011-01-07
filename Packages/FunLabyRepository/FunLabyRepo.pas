{*
  Gestion du repository de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyRepo;

interface

uses
  Windows, SysUtils, Classes, FunLabyUtils, FilesUtils, TortoiseGIT;

type
  {*
    Gestionnaire du repository FunLabyrinthe
    @author sjrd
    @version 5.2
  *}
  TFunLabyRepo = class(TObject)
  private
    FRepoPath: TFileName;       /// Path to the repository
    FTortoiseGIT: TTortoiseGIT; /// Interface avec TortoiseGIT

    FMasterFile: TMasterFile; /// Ma�tre FunLabyrinthe (peut �tre nil)

    property TortoiseGIT: TTortoiseGIT read FTortoiseGIT;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Commit;
    procedure Diff;
    procedure ShowLog;
    procedure Pull;
    procedure Publish;

    property RepoPath: TFileName read FRepoPath;
    property MasterFile: TMasterFile read FMasterFile write FMasterFile;
  end;

implementation

{--------------------}
{ TFunLabyRepo class }
{--------------------}

{*
  Cr�e un nouveau gestionnaire du repository FunLabyrinthe
*}
constructor TFunLabyRepo.Create;
begin
  inherited Create;

  FRepoPath := fFunLabyAppData;
  FTortoiseGIT := TTortoiseGIT.Create(FRepoPath);
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyRepo.Destroy;
begin
  FTortoiseGIT.Free;

  inherited;
end;

{*
  Commit
*}
procedure TFunLabyRepo.Commit;
begin
  TortoiseGIT.Commit;
end;

{*
  Affiche les diff�rences entre la working copy et HEAD
*}
procedure TFunLabyRepo.Diff;
begin
  // TODO
end;

{*
  Affiche le log du repository
*}
procedure TFunLabyRepo.ShowLog;
begin
  TortoiseGIT.ShowLog;
end;

{*
  Met � jour la copie locale depuis Internet
*}
procedure TFunLabyRepo.Pull;
begin
  TortoiseGIT.Pull;
end;

{*
  Publie un patch des modifications locales
*}
procedure TFunLabyRepo.Publish;
begin
  TortoiseGIT.CreatePatch;
end;

end.


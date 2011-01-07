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

    FMasterFile: TMasterFile; /// Maître FunLabyrinthe (peut être nil)

    property TortoiseGIT: TTortoiseGIT read FTortoiseGIT;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowLog;

    property RepoPath: TFileName read FRepoPath;
    property MasterFile: TMasterFile read FMasterFile write FMasterFile;
  end;

implementation

{--------------------}
{ TFunLabyRepo class }
{--------------------}

{*
  Crée un nouveau gestionnaire du repository FunLabyrinthe
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
  Affiche le log du repository
*}
procedure TFunLabyRepo.ShowLog;
begin
  TortoiseGIT.ShowLog;
end;

end.


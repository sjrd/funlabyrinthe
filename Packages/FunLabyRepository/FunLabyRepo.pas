{*
  Gestion du repository de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyRepo;

interface

uses
  Windows, SysUtils, Classes, FunLabyUtils, FilesUtils;

type
  TFunLabyRepo = class(TObject)
  private
    FMasterFile: TMasterFile; /// Ma�tre FunLabyrinthe (peut �tre nil)
  public
    property MasterFile: TMasterFile read FMasterFile write FMasterFile;
  end;

implementation

end.


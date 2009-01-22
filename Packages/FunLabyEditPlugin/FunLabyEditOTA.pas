{*
  FunLabyEdit Open Tools API
  @author sjrd
  @version 5.0
*}
unit FunLabyEditOTA;

interface

uses
  ScUtils, FunLabyUtils, FilesUtils, SepiCompilerErrors, SourceEditors;

type
  {*
    Bo�te de dialogue Messages du compilateur
    @author sjrd
    @version 5.0
  *}
  IOTACompilerMessages50 = interface
    ['{1F03DE7D-E8D1-4C76-8A2B-E598624F33AA}']

    {*
      Indique si les messages sont visibles
      @param True s'ils sont visibles, False sinon
    *}
    function GetVisible: Boolean;

    {*
      Rend visibles ou invisibles les messages
      @param Value   True pour les montrer, False pour les cacher
    *}
    procedure SetVisible(Value: Boolean);

    {*
      Erreurs de compilation
    *}
    function GetErrors: TSepiCompilerErrorList;

    {*
      Efface tous les messages du compilateur
    *}
    procedure Clear;

    {*
      Montre le premier message important
    *}
    procedure ShowFirst;

    property Visible: Boolean read GetVisible write SetVisible;

    property Errors: TSepiCompilerErrorList read GetErrors;
  end;

  {*
    Visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  IOTAMapViewer50 = interface
    ['{6E34CD43-98B6-4FD3-9B8C-E2842CFCC673}']

    {*
      Indique si le visualisateur est visible
      @param True s'il est visible, False sinon
    *}
    function GetVisible: Boolean;

    {*
      Rend visible ou invisible le visualisateur
      @param Value   True pour le montrer, False pour le cacher
    *}
    procedure SetVisible(Value: Boolean);

    {*
      Case s�lectionn�e
      @return Case s�lectionn�e
    *}
    function GetSelectedSquare: TQualifiedPos;

    {*
      Carte s�lectionn�e
      @return Carte s�lectionn�e
    *}
    function GetSelectedMap: TMap;

    {*
      Position s�lectionn�e
      @return Position s�lectionn�e
    *}
    function GetSelectedPos: T3DPoint;

    {*
      Modifie la case s�lectionn�e
      @param Value   Nouvelle case s�lectionn�e
    *}
    procedure SetSelectedSquare(const Value: TQualifiedPos);

    {*
      Modifie la carte s�lectionn�e
      @param Value   Nouvelle carte s�lectionn�e
    *}
    procedure SetSelectedMap(Value: TMap);

    {*
      Modifie la position s�lectionn�e
      @param Value   Nouvelle position s�lectionn�e
    *}
    procedure SetSelectedPos(const Value: T3DPoint);

    property Visible: Boolean read GetVisible write SetVisible;

    property SelectedSquare: TQualifiedPos
      read GetSelectedSquare write SetSelectedSquare;
    property SelectedMap: TMap
      read GetSelectedMap write SetSelectedMap;
    property SelectedPos: T3DPoint
      read GetSelectedPos write SetSelectedPos;
  end;

  {*
    Fen�tre principale de l'�diteur
    @author sjrd
    @version 5.0
  *}
  IOTAFunLabyEditMainForm50 = interface
    ['{FE189D28-4B90-4B51-80DF-9E987E205BAD}']

    {*
      Fichier ma�tre FunLabyrinthe
      @return Fichier ma�tre FunLabyrinthe
    *}
    function GetMasterFile: TMasterFile;

    {*
      Bo�te de dialogue Messages du compilateur
      @return Bo�te de dialogue Messages du compilateur
    *}
    function GetCompilerMessages: IOTACompilerMessages50;

    {*
      Visualisateur de cartes
      @return Visualisateur de cartes
    *}
    function GetMapViewer: IOTAMapViewer50;

    property MasterFile: TMasterFile read GetMasterFile;
    property CompilerMessages: IOTACompilerMessages50 read GetCompilerMessages;
    property MapViewer: IOTAMapViewer50 read GetMapViewer;
  end;

  {*
    �diteur de source qui utilise les OTA FunLabyEdit
    @author sjrd
    @version 5.0
  *}
  ISourceEditorUsingOTA50 = interface(ISourceEditor50)
    ['{D8DB5DEA-B8D3-4951-AF0D-6C789EC3C9EF}']

    {*
      Renseigne la fiche principale de FunLabyEdit
      @param MainForm   Fiche principale de FunLabyEdit
    *}
    procedure SetFunLabyEditMainForm(const MainForm: IOTAFunLabyEditMainForm50);
  end;

implementation

end.


{*
  FunLabyEdit Open Tools API
  @author sjrd
  @version 5.0
*}
unit FunLabyEditOTA;

interface

uses
  FunLabyUtils, SepiCompilerErrors, SourceEditors;

type
  {*
    Boîte de dialogue Messages du compilateur
    @author sjrd
    @version 5.0
  *}
  IOTACompilerMessages50 = interface
    ['{1F03DE7D-E8D1-4C76-8A2B-E598624F33AA}']

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

    property Errors: TSepiCompilerErrorList read GetErrors;
  end;

  {*
    Visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  IOTAMapViewer50 = interface
    ['{6E34CD43-98B6-4FD3-9B8C-E2842CFCC673}']
  end;

  {*
    Fenêtre principale de l'éditeur
    @author sjrd
    @version 5.0
  *}
  IOTAFunLabyEditMainForm50 = interface
    ['{FE189D28-4B90-4B51-80DF-9E987E205BAD}']

    {*
      Boîte de dialogue Messages du compilateur
      @return Boîte de dialogue Messages du compilateur
    *}
    function GetCompilerMessages: IOTACompilerMessages50;

    {*
      Visualisateur de cartes
      @return Visualisateur de cartes
    *}
    function GetMapViewer: IOTAMapViewer50;

    property CompilerMessages: IOTACompilerMessages50 read GetCompilerMessages;
    property MapViewer: IOTAMapViewer50 read GetMapViewer;
  end;

  {*
    Éditeur de source qui utilise les OTA FunLabyEdit
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


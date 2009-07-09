{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  FunLabyAuthorName = 'S�bastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://sjrd.developpez.com/programmes/funlaby/'; /// Site

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstru� par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';

  // Erreurs de composants
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SUnsupportedCommand = 'La commande %s n''est pas support�e';
  sTemporaryStatedMap =
    'La carte d''ID %s est dans un �tat temporaire qui ne peut �tre enregistr�';

  // Titres des messages
  SDescription = 'Description';
  SMessage = 'Message';
  STip = 'Indice';
  SChoice = 'Choix';
  SError = 'Erreur';
  SFailure = '�chec';
  SBlindAlley = 'Impasse';
  SWon = 'Gagn� !';
  SLost = 'Perdu !';

  // Formats de fichier et traitement des fichiers
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a �t� enregistr� avec une version ult�rieure '+
    'de FunLabyrinthe (v%s). Il ne peut �tre ouvert.';
  sFileNotFound = 'Le fichier sp�cifi� "%s" n''existe pas';
  sUnknownUnitType = 'Type d''unit� ''%s'' inconnu';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''�dition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''�dition d''une sauvegarde est impossible';
  sSourcesNotHandledWhilePlaying =
    'Les fichiers source ne sont pas g�r�s en mode jeu';
  sNoFileName = 'Aucun nom de fichier sp�cifi�';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';

implementation

end.


{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  FunLabyAuthorName = 'Sébastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://sjrd.developpez.com/programmes/funlaby/'; /// Site

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstrué par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';

  // Erreurs de composants
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SUnsupportedCommand = 'La commande %s n''est pas supportée';
  sTemporaryStatedMap =
    'La carte d''ID %s est dans un état temporaire qui ne peut être enregistré';

  // Titres des messages
  SDescription = 'Description';
  SMessage = 'Message';
  STip = 'Indice';
  SChoice = 'Choix';
  SError = 'Erreur';
  SFailure = 'Échec';
  SBlindAlley = 'Impasse';
  SWon = 'Gagné !';
  SLost = 'Perdu !';

  // Formats de fichier et traitement des fichiers
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  sFileNotFound = 'Le fichier spécifié "%s" n''existe pas';
  sUnknownUnitType = 'Type d''unité ''%s'' inconnu';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';
  sSourcesNotHandledWhilePlaying =
    'Les fichiers source ne sont pas gérés en mode jeu';
  sNoFileName = 'Aucun nom de fichier spécifié';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';

implementation

end.


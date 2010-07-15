{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.0';  /// Version courante de FunLabyrinthe
  CurrentMinorVersion = 2; /// Version mineure courante de FunLabyrinthe
  FunLabyAuthorName = 'Sébastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://funlabyrinthe.game-host.org/'; /// Site

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstrué par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';
  SDefaultPlayerName = 'Joueur';
  SComponentCreatorHint = 'Créer un nouveau composant de ce type';

  // Noms des catégories standard
  SCategoryFields = 'Terrains';
  SCategoryEffects = 'Effets';
  SCategoryTools = 'Outils';
  SCategoryObstacles = 'Obstacles';
  SCategoryNeutrals = 'Neutres';
  SCategoryVehicles = 'Véhicules';
  SCategoryPlayers = 'Joueurs';
  SCategoryObjects = 'Objets';
  SCategoryPlugins = 'Plug-in';
  SCategoryComponentCreators = 'Composants supplémentaires';

  // Erreurs de composants
  SComponentNotFoundTitle = 'Composant non trouvé';
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SInvalidComponentTitle = 'Composant invalide';
  SInvalidComponent = 'Le composant d''ID %s n''est pas valide';
  SRepareComponentError =
    'Voulez-vous ignorer cette erreur et tenter de continuer ?';
  SUnsupportedCommand = 'La commande %s n''est pas supportée';
  STemporaryStatedMap =
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
  SFullVersionNumber = '%s (build %d)';
  SInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  SVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  SFileNotFound = 'Le fichier spécifié "%s" n''existe pas';
  SUnknownUnitType = 'Type d''unité ''%s'' inconnu';
  SThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  SEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  SCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';
  SSourcesNotHandledWhilePlaying =
    'Les fichiers source ne sont pas gérés en mode jeu';
  SNoFileName = 'Aucun nom de fichier spécifié';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';

implementation

end.


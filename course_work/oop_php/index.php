<?php
  // Start Session
  session_start();

  // Include Config
  require(__DIR__ . '/config.php');

  require(__DIR__ . '/classes/Messages.php');
  require(__DIR__ . '/classes/Bootstrap.php');
  require(__DIR__ . '/classes/Controller.php');
  require(__DIR__ . '/classes/Model.php');

  require(__DIR__ . '/controllers/home.php');
  require(__DIR__ . '/controllers/users.php');
  require(__DIR__ . '/controllers/shares.php');

  require(__DIR__ . '/models/home.php');
  require(__DIR__ . '/models/user.php');
  require(__DIR__ . '/models/share.php');

  $bootstrap = new Bootstrap($_GET);
  $controller = $bootstrap->createController();
  if ($controller) {
    $controller->executeAction();
  }
?>

<?php
  class Shares extends Controller {
    protected function index() {
      $viewmodel = new ShareModel();
      $this->getView($viewmodel->index(), true);
    }

    protected function add() {
      if (!isset($_SESSION['is_logged_in'])) {
        header('Location: ' . ROOT_URL . 'shares');
      }

      $viewmodel = new ShareModel();
      $this->getView($viewmodel->add(), true);
    }
  }
?>

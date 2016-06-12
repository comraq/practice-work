<?php
  class Home extends Controller {
    protected function index() {
      $viewmodel = new HomeModel();
      $this->getView($viewmodel->index(), true);
    }
  }
?>

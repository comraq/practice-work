<?php
  class ShareModel extends Model {
    public function index() {
      $this->query('SELECT * FROM Shares ORDER BY create_date DESC');
      $rows = $this->resultSet();
      return $rows;
    }

    public function add() {
      // Sanitize POST
      $post = filter_input_array(INPUT_POST, FILTER_SANITIZE_STRING);

      if ($post['submit']) {
        if ($post['title'] == ''
            || $post['body'] == ''
            || $post['link'] == '') {
          Messages::setMsg('Please Fill in All Fields', 'error');
          return;
        }

        // Insert into MySQL Database
        $this->query('INSERT INTO Shares (title, body, link, user_id)
                      VALUES(:title, :body, :link, :user_id)');
        $this->bind(':title', $post['title']);
        $this->bind(':body', $post['body']);
        $this->bind(':link', $post['link']);
        $this->bind(':user_id', $_SESSION['user_data']['id']);
        $this->execute();

        // Verify Query
        if($this->lastInsertId()) {
          // Redirect
          header('Location: ' . ROOT_URL . 'shares');
        }
      }
      return;
    }
  }
?>

<?php
  class UserModel extends Model {
    public function register() {
      // Sanitize POST
      $post = filter_input_array(INPUT_POST, FILTER_SANITIZE_STRING);
      $password = md5($post['password']);

      if ($post['submit']) {
        if ($post['name'] == ''
            || $post['email'] == ''
            || $post['password'] == '') {
          Messages::setMsg('Please Fill in All Fields', 'error');
          return;
        }

        // Insert into MySQL Database
        $this->query('INSERT INTO Users (name, email, password)
                      VALUES(:name, :email, :password)');
        $this->bind(':name', $post['name']);
        $this->bind(':email', $post['email']);
        $this->bind(':password', $password);
        $this->execute();

        // Verify Query
        if($this->lastInsertId()) {
          // Redirect
          header('Location: ' . ROOT_URL . 'users/login');
        }
      }
      return;
    }

    public function login() {
      // Sanitize POST
      $post = filter_input_array(INPUT_POST, FILTER_SANITIZE_STRING);
      $password = md5($post['password']);

      if ($post['submit']) {
        // Check Login
        $this->query('SELECT * FROM Users where email = :email
                      AND password = :password');
        $this->bind(':email', $post['email']);
        $this->bind(':password', $password);

        $row = $this->single();
        if ($row) {
          $_SESSION['is_logged_in'] = true;
          $_SESSION['user_data'] = array(
            'id' => $row['id'],
            'name' => $row['name'],
            'email' => $row['email']
          );
          header('Location: ' . ROOT_URL . 'shares');

        } else {
          Messages::setMsg('Incorrect Login', 'error');
        }
      }
      return;
    }
  }
?>

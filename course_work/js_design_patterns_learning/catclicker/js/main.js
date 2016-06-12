(function(window, document) {
  var defaultCatIndex = 0;
  var controller = controller();
  controller.init();

  function controller() {
    // Static
    var pics = [
      "https://lh3.ggpht.com/nlI91wYNCrjjNy5f-S3CmVehIBM4cprx-JFWOztLk7vFlhYuFR6YnxcT446AvxYg4Ab7M1Fy0twaOCWYcUk=s0#w=640&h=426",
      "https://lh3.ggpht.com/kixazxoJ2ufl3ACj2I85Xsy-Rfog97BM75ZiLaX02KgeYramAEqlEHqPC3rKqdQj4C1VFnXXryadFs1J9A=s0#w=640&h=496",
      "https://lh5.ggpht.com/LfjkdmOKkGLvCt-VuRlWGjAjXqTBrPjRsokTNKBtCh8IFPRetGaXIpTQGE2e7ZCUaG2azKNkz38KkbM_emA=s0#w=640&h=454",
      "http://i.dailymail.co.uk/i/pix/2015/10/20/01/2D92587600000578-3280211-So_hot_right_meow_A_Japanese_housewife_creates_handmade_cat_purs-m-62_1445301682918.jpg",
      "https://www.petfinder.com/wp-content/uploads/2012/11/140272627-grooming-needs-senior-cat-632x475.jpg"
    ];

    // Models
    var cats = [];
    var currentCat = {};

    // Views
    var listView, displayView, adminView;

    // Interface exposed to views
    var controllerInterface = {
      listItemClickListener: function listItemClickListener(name) {
        for (var i = 0; i < cats.length; ++i) {
          if (cats[i].name == name) {
            setCurrentCat(cats[i]);
            return renderAll();
          }
        }
      },
      imgClickListener: function imgClickListener() {
        currentCat.incCount();
        return renderAll();
      },
      getCat: function getCat() {
        return {
          name: currentCat.name,
          imgUrl: currentCat.imgUrl,
          count: currentCat.getCount()
        }
      },
      updateCatInfo: function updateCatInfo(updatedCat) {
        setCurrentCat(updatedCat);
        return renderAll();
      }
    }

    function renderAll() {
      displayView.render(controllerInterface.getCat());
      adminView.render();
    }

    function setCurrentCat(cat) {
      currentCat.name = cat.name;
      currentCat.imgUrl = cat.imgUrl;
      currentCat.incCount = function incCount() {
        cat.incCount();
        updateLevel(cat.getCount());
      };
      currentCat.getCount = cat.getCount;
      currentCat.setCount = function setCount(newCount) {
        cat.setCount(newCount);
        updateLevel(cat.getCount());
      };

      updateLevel(cat.getCount());
    }

    var stage = ko.observable("Newborn");
    function updateLevel(count) {
      if (count < 10)
        stage("Newborn");

      else if (count < 30)
        stage("Infant");

      else if (count < 50)
        stage("Child");

      else if (count < 70)
        stage("Teen");

      else if (count < 90)
        stage("Adult");

      else
        stage("Senior");
    }

    return {
      init: function init() {
        // Generating an array of cat models
        pics.forEach(function(imgUrl, i, arr) {
          cats.push(catModelFactory("Cat " + (i + 1), imgUrl));
        });

        displayView = displayViewFactory(controllerInterface);
        listView = listViewFactory(controllerInterface);
        adminView = adminViewFactory(controllerInterface);

        displayView.init();
        listView.init(cats.map(function(cat) { return cat.name }));
        adminView.init();

        var myVM = {
          nicknames: [ "a", "b", "c", "d" ],
          level: stage
        };
        ko.applyBindings(myVM);

      }
    };
  }

  function listViewFactory(controller) {
    var groupView, listView, titleView;
    return {
      init: function init(cats) {
        groupView = document.getElementById("list-group");
        for (var i = 0; i < groupView.children.length; ++i) {
          e = groupView.children[i];
          switch (e.id) {
            case "title":
              titleView = e;
              break;

            case "list":
              listView = e;
              break;
      
            default:
              // Nothing
          }
        }
        titleView.innerHTML = "Cats List:";
        cats.forEach(function(cat, i, arr) {
          var li = document.createElement("li");
          li.innerHTML = cat;
          li.addEventListener("click", function() {
            return controller.listItemClickListener(cat);
          });
          listView.appendChild(li);
        });
        listView.children[defaultCatIndex].click();
      }
    };
  }

  function displayViewFactory(controller) {
    var groupView, countView, imgView, nameView;
 
    return {
      init: function init() {
        groupView = document.getElementById("display-group");
        for (var i = 0; i < groupView.children.length; ++i) {
          e = groupView.children[i];
          switch (e.id) {
            case "name":
              nameView = e;
              break;

            case "cat-img":
              imgView = e;
              break;
      
            case "counter":
              countView = e.getElementsByClassName("value")[0];     
              break;
      
            default:
              // Nothing
          }
        }
        imgView.addEventListener("click",
                                 controller.imgClickListener,
                                 false);
      },
      render: function render(cat) {
        nameView.innerHTML = cat.name;
        imgView.setAttribute("src", cat.imgUrl);
        countView.innerHTML = cat.count;
      }
    };
  }

  function adminViewFactory(controller) {
    var groupView, adminButton, cancelButton, formView;

    var adminStatus = false;

    function adminToggle(status) {
      if (status) {
        adminStatus = true;
        adminButton.children[0].innerHTML = "On";
        formView.style.display = "inline-block";
      } else {
        adminStatus = false;
        adminButton.children[0].innerHTML = "Off";
        formView.style.display = "none";
      }
    }

    function showForm(cat) {
      var nameInput = formView["name"];
      var imgUrlInput = formView["img-url"];
      var countInput = formView["counter"];

      nameInput.value = cat.name;
      imgUrlInput.value = cat.imgUrl;
      countInput.value = cat.count;
    }

    return {
      init: function init() {
        groupView = document.getElementById("admin-group");
        for (var i = 0; i < groupView.children.length; ++i) {
          e = groupView.children[i];
          switch (e.id) {
            case "admin-button":
              adminButton = e;
              break;

            case "admin-form":
              formView = e;
              break;
      
            default:
              // Nothing
          }
        }
        adminButton.addEventListener("click", function() {
          adminToggle(true);
          showForm(controller.getCat());
        });

        var submitButton, cancelButton;
        for (var i = 0; i < formView.children.length; ++i) {
          e = formView.children[i];
          switch (e.id) {
            case "cancel-button":
              cancelButton = e;
              break;
      
            case "submit-button":
              submitButton = e;
              break;

            default:
              // Nothing
          }
        }
        cancelButton.addEventListener("click", function() {
          adminToggle(false);
        });
        submitButton.addEventListener("click", function() {
          var name = formView["name"].value;
          var imgUrl = formView["img-url"].value;
          var count = parseInt(formView["counter"].value);
          if (isNaN(count) || count < 0) {
            alert("Invalid cat click count!")
            return;
          }

          controller.updateCatInfo({
            name: name,
            imgUrl: imgUrl,
            count: count
          });
        });

        cancelButton.click();
      },
      render: function render() {
        if (adminStatus)
          adminButton.click();
      }
    };
  }

  function catModelFactory(name, imgUrl) {
    var count = 0;

    return {
      name: name,
      imgUrl: imgUrl,
      getCount: function getCount() { return count; },
      setCount: function setCount(newCount) {
        var newVal = parseInt(newCount);
        if (!isNaN(newVal)) {
          //count = newVal;
          count(newVal);
        } else
          console.log("Invalid Click Count to Set!");
      },
      incCount: function incCount() { ++count; }
    };
  }

})(window, document);

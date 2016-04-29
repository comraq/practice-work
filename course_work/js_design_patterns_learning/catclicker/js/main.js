(function(window, document) {
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
    var cats;

    // Views
    var listView, displayView;

    // Interface exposed to views
    var controllerInterface = {
      listItemClickListener: function listItemClickListener(name) {
        for (var i = 0; i < cats.length; ++i) {
          if (cats[i].name == name) {
            return displayView.render(cats[i].name,
                                      cats[i].imgUrl,
                                      cats[i].getCount());
          }
        }
      },
      imgClickListener: function imgClickListener(name) {
        for (var i = 0; i < cats.length; ++i) {
          if (cats[i].name == name) {
            cats[i].incCount();
            return displayView.renderCount(cats[i].getCount());
          }
        }
      },

    }

    return {
      init: function init() {
        // Generating an array of cat models
        cats = pics.map(function(imgUrl, i, arr) {
          return catModelFactory("Cat " + (i + 1), imgUrl);
        });

        displayView = displayViewFactory(controllerInterface);
        listView = listViewFactory(controllerInterface);

        displayView.init();
        listView.init(cats.map(function(cat) { return cat.name }));
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
        listView.children[0].click();
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
          function () {
            return controller.imgClickListener(nameView.innerHTML);
          }, false);
      },
      render: function render(name, imgUrl, count) {
        nameView.innerHTML = name;
        imgView.setAttribute("src", imgUrl);
        countView.innerHTML = count;
      },
      renderCount: function renderCount(count) {
        countView.innerHTML = count;
      }
    };
  }

  function catModelFactory(name, imgUrl) {
    var count = 0;

    return {
      name: name,
      imgUrl: imgUrl,
      getCount: function getCount() { return count; },
      incCount: function incCount() { ++count; }
    };
  }
})(window, document);

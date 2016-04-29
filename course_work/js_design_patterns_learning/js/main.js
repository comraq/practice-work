(function(window, document) {
  this.pics = [
    "https://lh3.ggpht.com/nlI91wYNCrjjNy5f-S3CmVehIBM4cprx-JFWOztLk7vFlhYuFR6YnxcT446AvxYg4Ab7M1Fy0twaOCWYcUk=s0#w=640&h=426",
    "https://lh3.ggpht.com/kixazxoJ2ufl3ACj2I85Xsy-Rfog97BM75ZiLaX02KgeYramAEqlEHqPC3rKqdQj4C1VFnXXryadFs1J9A=s0#w=640&h=496",
    "https://lh5.ggpht.com/LfjkdmOKkGLvCt-VuRlWGjAjXqTBrPjRsokTNKBtCh8IFPRetGaXIpTQGE2e7ZCUaG2azKNkz38KkbM_emA=s0#w=640&h=454",
    "http://i.dailymail.co.uk/i/pix/2015/10/20/01/2D92587600000578-3280211-So_hot_right_meow_A_Japanese_housewife_creates_handmade_cat_purs-m-62_1445301682918.jpg",
    "https://www.petfinder.com/wp-content/uploads/2012/11/140272627-grooming-needs-senior-cat-632x475.jpg"
  ];

  var current = {};
  var cats = document.getElementById("list");
  pics.forEach(function(img, index, arr) {
    // Cat Object Schema
    var cat = {
      name: "Cat " + index,
      img: img,
      count: 0,
      id: index
    };

    var li = document.createElement("li");
    li.innerHTML = cat.name;
    li.addEventListener("click", function() {
      current.id = cat.id;

      showCatDetails(
        document.getElementById("display-group"),
        cat
      );
    }, false);

    initCatListener(
      document.getElementById("display-group"),
      cat,
      incCounter);

    // Add Each Cat to List
    cats.appendChild(li)
  });

  cats.children[0].click();

  function incCounter(count, cat) {
    return function() {
      if (current.id == cat.id)
        count.innerHTML = ++cat.count;
    };
  }
})(window, document);

function showCatDetails(group, selectedCat) {
  var name, count, img;
  for (var i = 0; i < group.children.length; ++i) {
    e = group.children[i];
    switch (e.id) {
      case "name":
        name = e;
        break;

      case "cat-img":
        img = e;
        break;

      case "counter":
        count = e.getElementsByClassName("value")[0];     
        break;

      default:
        // Nothing
    }
  }

  name.innerHTML = selectedCat.name;
  img.setAttribute("src", selectedCat.img);
  count.innerHTML = selectedCat.count;
}

function initCatListener(group, selectedCat, clickListener) {
  var count, img;
  for (var i = 0; i < group.children.length; ++i) {
    e = group.children[i];
    switch (e.id) {
      case "cat-img":
        img = e;
        break;

      case "counter":
        count = e.getElementsByClassName("value")[0];     
        break;

      default:
        // Nothing
    }
  }
  img.addEventListener("click", clickListener(count, selectedCat), false);
}

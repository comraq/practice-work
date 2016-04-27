var ListNode = require("./ListNode");

var oddEvenList = function(head) {
  var oddStart = null,
      evenStart = null,
      oddP = null,
      evenP = null,
      even = false;
  while (head !== null) {
    if (even) {
      if (!evenStart) {
        evenStart = head;
        evenP = evenStart;
      } else {
        evenP.next = head;
        evenP = evenP.next;
      }
    } else {
      if (!oddStart) {
        oddStart = head;
        oddP = oddStart;
      } else {
        oddP.next = head;
        oddP = oddP.next;
      }
    }
    head = head.next; 
    even = !even;
  }
  if (oddP !== null) {
    oddP.next = evenStart;
    if (evenP !== null)
      evenP.next = null;
  }
  return oddStart;
};

(function main() {
  var head = ListNode([1,2,3,4,5,6,7,8]);
  head = oddEvenList(head);
  while (head !== null) {
    console.log(head.val);
    head = head.next;
  }
})();

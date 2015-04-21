# WebSharper UI.Next TodoMVC Implementation

An implementation of [TodoMVC](http://www.todomvc.com/) using [WebSharper UI.Next](http://intellifactory.github.io/websharper.ui.next).

# TODO

The implementation does not fully conform to the [TodoMVC app specification](https://github.com/tastejs/todomvc/blob/master/app-spec.md):

https://github.com/tastejs/todomvc/blob/master/app-spec.md

* The routing scheme is slightly different from what is defined in the specs (this would need changes in ui.next itself).
* There is currently no model implementation that is backed by local storage. It may be worth extracting the in-memory array that is used by `ListModel` and provide a way to plug the actual store implementation.
* When the user double-clicks the label of an item `focus()` is not called on the element as it's not possible to get a reference to the right input box within `UI.Next`.
* The mark-up is slightly different (differs on 3 parts: +2 spans and +1 div) because of the nature of how our templating works.
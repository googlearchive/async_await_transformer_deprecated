import 'dart:collection';

abstract class Result {
  bool moveNext(Flatten flatten);
}

class FromIterator extends Result {
  var _iterator;
  FromIterator(this._iterator);
  bool moveNext(Flatten flatten) {
    if(_iterator.moveNext()) {
      flatten._stack.add(() => this);
      flatten._current = _iterator.current;
      return true;
    } else {
      return flatten.moveNext();
    }
  }
}

class Done extends Result {
  bool moveNext(Flatten flatten) {
    return flatten.moveNext();
  }
}

class Single extends Result {
  var _current, _moveNext;
  Single(this._current, this._moveNext);
  bool moveNext(Flatten flatten) {
     flatten._stack.add(_moveNext);
     flatten._current = _current;
     return true;
  }
}

class Nested extends Result {
  var _current, _moveNext;
  Nested(this._current, this._moveNext);
  bool moveNext(Flatten flatten) {
     flatten._stack.add(_moveNext);
     // make sure that we have a SyncStar
     flatten._stack.add(new SyncStar.fromIterable(_current)._iterator);
     return flatten.moveNext();
 }
}

class Tail extends Result {
  var _current;
  Tail(this._current);
  bool moveNext(Flatten flatten) {
     // make sure that we have a SyncStar
     flatten._stack.add(new SyncStar.fromIterable(_current)._iterator);
     return flatten.moveNext();
   }
}

class SyncStar<E> extends IterableBase<E> {
  var _iterator;
  SyncStar(this._iterator);
  
  factory SyncStar.fromIterable(Iterable<E> iterable) {
    if(iterable is SyncStar) { return iterable; }
    return new SyncStar(() => new FromIterator(iterable.iterator));
  }
  
  Iterator<E> get iterator => new Flatten(_iterator);
}

class Flatten extends Iterator {
  Flatten(f){ _stack.add(f); }
  var _stack = [];
  
  var _current;
  get current => _current;

  bool moveNext() {
    if(_stack.length == 0){ return false; }
    var continuation = _stack.removeLast();
    var result = continuation();
    return result.moveNext(this);
  }
}
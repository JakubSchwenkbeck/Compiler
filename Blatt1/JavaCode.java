abstract Class BBaum<A> {
// Abstract class BBaum with generics

abstract<B> BBaum<B> mapTree(Function<A,B> f);
// Abstract method for subclasses to implement, using generics
}


class Empty<A> extends BBaum<A> {

@override // Overriding the mapTree to always return empty BBaum
public <B> BBaum<B> mapTree(Function<A,B> f){
	return new Empty <B>();
	}
}



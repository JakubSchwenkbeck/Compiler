#
abstract Class BBaum<A> {
// Abstract class BBaum with generics

abstract<B> BBaum<B> mapTree(Function<A,B> f);
// Abstract method for subclasses to implement, using generics
}



// empty Tree class
class Empty<A> extends BBaum<A> {

@override // Overriding the mapTree to always return empty BBaum
public <B> BBaum<B> mapTree(Function<A,B> f){
	return new Empty <B>();
	}
}

// Node class
class Node<A> extends BBaum<A> {
private A Value;
private BBaum<A> left;
private BBaum<A> right;



public Node<A>(A value,BBaum<A> left, BBaum<A> right){

	this.Value = Value;
	this.left = left;
	this.right = right;

}

@override <B> BBaum<B> mapTree(Function<A,B> f){
f.apply(Value, f.apply(left,right));
}

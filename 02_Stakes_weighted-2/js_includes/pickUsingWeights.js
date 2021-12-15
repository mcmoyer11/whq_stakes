/**
 * Takes an array of items and an array of the corresponding weights and picks
 * one of the items.  The items with the higher weights are more likely to be
 * picked.
 * @param {!Array} items  An array of items to choose from.
 * @param {!Array.<number>} weights  An array of non-negative integers with each
 *     number representing the likelihood of the number getting picked.
 * @return {*}  Returns one of the items from the array as long as at least one
 *     of the weights was a positive integer.  Otherwise whatever is at
 *     items[-1] will be returned (this usually means undefined is returned).
 */
function pickUsingWeights(items, weights) {
  var total = 0;
  var ranges = weights.slice(0);
  for(var i = 0, len = weights.length; i < len; i++) {
    ranges[i] = [total, total += ranges[i]];
  }
  var randomNumber = parseInt(Math.random() * total);
  for(;randomNumber < ranges[--i][0];);
  return items[i];
}

function pickUsingWeightsAndRemove(items, weights) {
  var total = 0;
  var ranges = weights.slice(0);
  for(var i = 0, len = weights.length; i < len; i++) {
    ranges[i] = [total, total += ranges[i]];
  }
  var randomNumber = parseInt(Math.random() * total);
  for(;randomNumber < ranges[--i][0];);

//   return items[i];  
  var pick = items[i];
  
  for(var i = items.length - 1; i >= 0; i--){
  	if (items[i] === pick){
  		items.splice(i,1);
  		weights.splice(i,1);
  	}
  }
  
  return pick;
}
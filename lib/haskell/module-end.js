
  global.getPackage = function() {
    var result = null;
    var callback = function (res) {
      result = res;
    };
    var action = h$c2(
      h$ap1_e,
      h$mainZCMainzigetPackage,
      h$c1(h$ghcjszmprimZCGHCJSziPrimziJSRef_con_e, callback)
    );
    h$runSync(action, false);
    return result;
  }

})(exports);

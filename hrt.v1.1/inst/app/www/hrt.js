

$(document).ready(function () {
    $("input").click(function () {
        var elts = ["Nucleus", "Microtubules", "Target protein", "ER"]
        var myitem = $(this).attr("value") 

        console.log("Item: ", myitem)

        if(elts.includes(myitem)) {

            var checkState = $(this).attr("checked")

            if(checkState == "checked"){

                alert(myitem);

                $(this).removeAttr("checked");
                

            } else {
                $(this).attr("checked", "checked")
            }

            
    

        }
        
    });
});

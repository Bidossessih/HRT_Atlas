$(document).ready(function () {
    $("input").click(function () {
        var elts = ["Nucleus", "Microtubules", "Target protein", "ER"]
        var myitem = $(this).attr("value") 

        console.log("Item: ", myitem)

        if(elts.includes(myitem)) {

            var checkState = $(this).attr("checked")

            if(checkState == "checked"){

                $(this).removeAttr("checked");
                

            } else {
                $(this).attr("checked", "checked")
            }            
    

        }

        $('input').each(function () {
            var checkAll = [];
            checkAll.push($(this).attr('checked'));
            var ch = checkAll.includes("checked");
            
            console.log("checked: ", ch)

            /*if(!ch) {
                alert("Not checked")
            }*/
            
        });
        
    });

   
    
});




/*Open result page based on selected cell type */

function disabledSearch() {

    if($("#search").val() == "") {
        $("#buttonresult").addClass("disabled")

    } 
    
    if($("#search").val() != "") {
        $("#buttonresult").removeClass("disabled")
    }

};

document.getElementById("search").addEventListener("change", disabledSearch);


function openResult() {

    var cell = $("#search").val().replaceAll(" ", "-")

    var specie = window.location.search.replace("?", "")
    window.location.href="/?page=".concat(specie).concat("-housekeeping-gene&tissue=").concat(cell);
}


document.getElementById("buttonresult").addEventListener("click", openResult);
      



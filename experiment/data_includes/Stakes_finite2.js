//Finite Condition

var manualSendResults = "true";

//var shuffleSequence = seq("trial_template","sr","end");

//var shuffleSequence = seq("consent","instructions",
//                          "train_F","train_M",
//                          "control_why","control_what","control_how","control_when",
//                                   "high_1","high_2","high_3","high_4","high_5",
//                                   "low_1","low_2","low_3","low_4","low_5",
//                                   "sr","end");

var shuffleSequence = seq("consent","instructions",
                          randomize("train"),"train_end",
                          shuffle(randomize("control"), randomize("nst"),
                                           randomize("shoes","books","hair","dentist","yarn","coffee")),
                          "exit_quest",
                         "sr","end")


var items = [
    // controller to send results early
        ["sr", "__SendResults__", { }],
    //instructions, consent, training, and end display
        ["instructions", "Form", { html: {include: "instructions.html" } } ],
        ["consent", "Form", { html: {include: "consent.html" }, hideProgressBar: true, countsForProgressBar: false} ],
        ["train", "Form", { html: {include: "train_F.html" } } ],
        ["train", "Form", { html: {include: "train_M.html" } } ],
        ["train_end", "Form", { html: {include: "and_begin.html" } } ],
        ["exit_quest", "Form", { html: {include: "exit_questionnaire.html" } } ],
        ["end", "Form", { html: {include: "end.html" }, countsForProgressBar: false, continueMessage: null} ],
    // trials
        ["shoes", "Form", { html: {include: "shoes.html" } } ],
        ["books", "Form", { html: {include: "books.html" } } ],
        ["hair", "Form", { html: {include: "hair.html" } } ],
        ["dentist", "Form", { html: {include: "dentist.html" } } ],
        ["yarn", "Form", { html: {include: "florist.html" } } ],
        ["coffee", "Form", { html: {include: "coffee.html" } } ],
    // controls
        ["control", "Form", { html: {include: "control_why.html" } } ],
        ["control", "Form", { html: {include: "control_what.html" } } ],
        ["control", "Form", { html: {include: "control_when.html" } } ],
        ["control", "Form", { html: {include: "control_how.html" } } ],
    // native speaker tests
        ["nst", "Form", { html: {include: "nst_1.html" } } ],
        ["nst", "Form", { html: {include: "nst_2.html" } } ],
        ["nst", "Form", { html: {include: "nst_3.html" } } ],
        ["nst", "Form", { html: {include: "nst_4.html" } } ],

];

var defaults = [
    // save reaction time for each Form controller
    "Form", {saveReactionTime: "true"}
];

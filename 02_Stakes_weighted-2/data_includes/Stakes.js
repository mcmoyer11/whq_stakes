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
                          shuffle(randomize("control"),
                                           shuffle(randomize("high"),
                                                   randomize("low"))),
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
        ["high", "Form", { html: {include: "high_1.html" } } ],
        ["high", "Form", { html: {include: "high_2.html" } } ],
        ["high", "Form", { html: {include: "high_3.html" } } ],
        ["high", "Form", { html: {include: "high_4.html" } } ],
        ["high", "Form", { html: {include: "high_5.html" } } ],
        ["low", "Form", { html: {include: "low_1.html" } } ],
        ["low", "Form", { html: {include: "low_2.html" } } ],
        ["low", "Form", { html: {include: "low_3.html" } } ],
        ["low", "Form", { html: {include: "low_4.html" } } ],
        ["low", "Form", { html: {include: "low_5.html" } } ],
    // controls
        ["control", "Form", { html: {include: "control_why.html" } } ],
        ["control", "Form", { html: {include: "control_what.html" } } ],
        ["control", "Form", { html: {include: "control_when.html" } } ],
        ["control", "Form", { html: {include: "control_how.html" } } ],

];

var defaults = [
    // save reaction time for each Form controller
    "Form", {saveReactionTime: "true"}
];


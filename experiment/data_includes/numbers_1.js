//Finite Condition

var manualSendResults = "true";

var shuffleSequence = seq("consent","instructions",
                          randomize("train"),"train_end",
                          shuffle(randomize("control"),
                              shuffle(
                                    randomize("nst"),
                                    shuffle(
                                        "ma_tell",
                                        "ms_tell",
                                        "ma_know",
                                        "ms_know"
                                    )
                              )
                          ),
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
    // ma_tell
        ["ma_tell", "Form", { html: {include: "names_MA_tell.html" } } ],
        ["ma_tell", "Form", { html: {include: "planets_MA_tell.html" } } ],
        ["ma_tell", "Form", { html: {include: "primes_MA_tell.html" } } ],
        ["ma_tell", "Form", { html: {include: "vowels_MA_tell.html" } } ],
    // ms_tell
        ["ms_tell", "Form", { html: {include: "names_MS_tell.html" } } ],
        ["ms_tell", "Form", { html: {include: "planets_MS_tell.html" } } ],
        ["ms_tell", "Form", { html: {include: "primes_MS_tell.html" } } ],
        ["ms_tell", "Form", { html: {include: "vowels_MS_tell.html" } } ],
    // ma_know
        ["ma_know", "Form", { html: {include: "names_MA_know.html" } } ],
        ["ma_know", "Form", { html: {include: "planets_MA_know.html" } } ],
        ["ma_know", "Form", { html: {include: "primes_MA_know.html" } } ],
        ["ma_know", "Form", { html: {include: "vowels_MA_know.html" } } ],
    // ms_know
        ["ms_know", "Form", { html: {include: "names_MS_know.html" } } ],
        ["ms_know", "Form", { html: {include: "planets_MS_know.html" } } ],
        ["ms_know", "Form", { html: {include: "primes_MS_know.html" } } ],
        ["ms_know", "Form", { html: {include: "vowels_MS_know.html" } } ],
    // controls
        ["control", "Form", { html: {include: "control_why.html" } } ],
        ["control", "Form", { html: {include: "control_what.html" } } ],
        ["control", "Form", { html: {include: "control_when.html" } } ],
        ["control", "Form", { html: {include: "control_how.html" } } ],
    // ginzburg trials
        ["ginz", "Form", { html: {include: "ginz_largegrain_who.html" } } ],
        ["ginz", "Form", { html: {include: "ginz_largegrain_where.html" } } ],
        ["ginz", "Form", { html: {include: "ginz_smallgrain_who.html" } } ],
        ["ginz", "Form", { html: {include: "ginz_smallgrain_where.html" } } ],
    // native speaker tests
        ["nst", "Form", { html: {include: "nst_1.html" } } ],
        ["nst", "Form", { html: {include: "nst_2.html" } } ],
        ["nst", "Form", { html: {include: "nst_3.html" } } ],
        ["nst", "Form", { html: {include: "nst_4.html" } } ]

];

var defaults = [
    // save reaction time for each Form controller
    "Form", {saveReactionTime: "true"}
];

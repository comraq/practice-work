q2)

db.grades.aggregate([
  {
    $match: {
      $or: [
        {
          "scores": {
            $elemMatch: {
              "type": "homework"
            }
          }
        },
        {
          "scores": {
            $elemMatch: {
              "type": "exam"
            }
          }
        }
      ]
    }
  },
  { $unwind: "$scores" },
  { $match: { "scores.type": { $ne: "quiz" } } },
  {
    $group: {
      _id: { "class": "$class_id", "student": "$student_id" },
      "student_avg": { $avg: "$scores.score" }
    }
  },
  {
    $group: {
      _id: "$_id.class",
      "class_avg": { $avg: "$student_avg" }
    }
  },
  { $sort: { "class_avg": -1 } }
]);

q3)

db.companies.aggregate([
  {
    $match: {
      "founded_year": 2004
    }
  },
  {
    $project: {
      "gte_five": { $gte: [ { $size: "$funding_rounds" }, 5 ] },
      "funding_rounds": 1,
      "name": 1   
    }
  },
  { $match: { "gte_five": true } },
  { $unwind: "$funding_rounds" },
  {
    $group: {
      _id: "$name",
      "avg_funding": { $avg: "$funding_rounds.raised_amount" }
    }
  },
  { $sort: { "avg_funding": 1 } }
]);

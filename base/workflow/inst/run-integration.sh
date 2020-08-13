#!/usr/bin/env bash
set -euo pipefail

# Submit the runs first
for f in tests/api/*.xml; do
    FSUBMIT=${f/xml/submit.json}
    curl -s localhost:8000/api/workflows/ --user carya:illinois -H "Content-Type:application/xml" --data-binary "@$f" > $FSUBMIT
    WORKFLOWID=$(jq ".workflow_id" $FSUBMIT -r)
    echo "Submitted test $f with workflow ID $WORKFLOWID"
done

# Now, check the status of each run
ISCOMPLETE=1
until [ $ISCOMPLETE -eq 0 ]; do
    ISCOMPLETE=0
    for f in tests/api/*.submit.json; do
        WORKFLOWID=$(jq ".workflow_id" $f -r)
        FSTATUS=${f/submit.json/status.json}
        FOUT=${f/submit.json/workflow.Rout}
        curl -s localhost:8000/api/workflows/$WORKFLOWID/status --user carya:illinois > $FSTATUS
        if grep -q "FINISHED.*DONE" $FSTATUS; then
            echo "Workflow $WORKFLOWID completed successfully"
            curl -s localhost:8000/api/workflows/$WORKFLOWID/file/workflow.Rout > $FOUT
        elif grep -q "ERROR" $FSTATUS; then
            echo "Workflow $WORKFLOWID ended in error"
            curl -s localhost:8000/api/workflows/$WORKFLOWID/file/workflow.Rout > $FOUT
        else
            echo "Workflow $WORKFLOWID is still running."
            ISCOMPLETE=1
        fi
    done
    echo "-----------------"
    if [ $ISCOMPLETE -eq 1 ]; then
        sleep 15
    fi
done

echo "All runs have finished or errored."
exit 0

import os
import sys
from datetime import date

import synapseclient
import synapseutils as su
import pandas as pd
import feather


def _expand_fileinfo(syn, synId):
    entity = syn.getEntity(synId)
    fileinfo_fields = ['createdBy', 'modifiedBy', 'versionNumber']
    fileinfo = {k: v for k, v in entity.items()
                if k in fileinfo_fields}
   
    entity_filehandle = entity['_file_handle']
    filehandleinfo_fields = ['contentMd5', 'contentSize',
                             'externalURL', 'fileName']
    filehandleinfo = {k: v for k, v in entity_filehandle.items()
                      if k in filehandleinfo_fields}
    if filehandleinfo['fileName'] == 'NOT_SET':
        filehandleinfo['fileName'] = os.path.basename(
            filehandleinfo['externalURL']
        )
        filehandleinfo['downloadName'] = 'NOT_SET'
    else:
        filehandleinfo['downloadName'] = filehandleinfo['fileName']

    return {**fileinfo, **filehandleinfo}


def add_fileinfo(syn, df):
    fileinfo = _expand_fileinfo(syn, df['entityId'])
    for k in fileinfo:
        df[k] = fileinfo[k]
    return df


def _expand_userinfo(syn, userId):
    userinfo = syn.getUserProfile(userId)
    return {'User': userinfo['userName'],
            'Name': ' '.join([userinfo['firstName'], userinfo['lastName']])}
 

def add_userinfo(syn, df, usercol):
    userinfo = _expand_userinfo(syn, df[usercol])
    for k in userinfo:
        addcol = usercol + k
        df[addcol] = userinfo[k]
    return df


def synwalk_to_df(syn, synId):
    res = []
    for root, dirs, entities in su.walk(syn, synId):
        if len(entities) > 0:
            path_names = [root[0]]*len(entities)
            path_ids = [root[1]]*len(entities)
            entity_names = map(lambda x: x[0], entities)
            entity_ids = map(lambda x: x[1], entities)
            res.extend(zip(path_names, path_ids, entity_names, entity_ids))
    return pd.DataFrame(
            res, 
            columns=['folderPath', 'folderId', 'entityName', 'entityId']
        )

def get_project_path(synId, path=None):
     if path is None:
         path = ""
     entity_info = syn.getEntity(synId)
     if 'Project' in entity_info['entityType']:
         return os.path.dirname(os.path.join(entity_info['name'], path))
     else:
         path = os.path.join(entity_info['name'], path)
         return get_project_path(entity_info['parentId'], path)


def build_manifest(syn, synId):
    print("collecting list of entities in tree with root '{}'".format(synId))
    base_df = synwalk_to_df(syn, synId)
    print("{} entities found".format(len(base_df.index)))
    print("expanding file and user details for all entities")
    manifest_df = (
        base_df
        .apply(lambda x: add_fileinfo(syn, x), axis=1)
        .apply(lambda x: add_userinfo(syn, x, 'createdBy'), axis=1)
        .apply(lambda x: add_userinfo(syn, x, 'modifiedBy'), axis=1)
        .rename(columns={'createdBy': 'createdById', 
                         'modifiedBy': 'modifiedById'})
    )
    return manifest_df


def save_manifest(df, synId, out_dir=None):
    path = '{}_manifest_{}.feather'.format(
        synId,
        date.isoformat(date.today())
    )
    if out_dir:
        path = os.path.join(out_dir, path)
        if not os.path.exists(out_dir):
            os.makedirs(out_dir)
    print("saving results to '{}'".format(path))
    feather.write_dataframe(df, path)


def main(argv):
    syn = synapseclient.Synapse()
    syn.login()
    root_id = argv[0]
    if len(argv) > 1:
        out_dir = argv[1]
    else:
        out_dir = None
    save_manifest(build_manifest(syn, root_id), root_id, out_dir)


if __name__ == "__main__":
    main(sys.argv[1:])

